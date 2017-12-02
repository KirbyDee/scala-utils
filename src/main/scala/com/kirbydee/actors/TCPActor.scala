package com.kirbydee.actors

import java.net.InetSocketAddress

import com.kirbydee.utils.Implicits._
import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.kirbydee.logr.Logr

import scala.concurrent.duration.FiniteDuration

/**
  * TCP Actor Trait that can be used to instantiate a TCP connection as a client.
  */
trait TCPActor extends Actor with Logr {
    import context.system

    // response object from TCP connection
    abstract class TCPResponse
    case object TimeoutFailure extends TCPResponse
    case object ConnectionFailure extends TCPResponse
    case object WriteFailure extends TCPResponse
    case class ReadSuccess(message: ByteString) extends TCPResponse

    // TCP connection information
    val host: String
    val port: Int
    val message: ByteString
    val timeout: FiniteDuration

    // address of the TCP socket
    val address = new InetSocketAddress(host, port)

    // open connection
    IO(Tcp) ! Connect(
        remoteAddress = this.address,
        timeout       = Some(this.timeout)
    )

    /**
      * Overridden receive method from Actor.
      * This method takes care of all the messages received by the actor.
      */
    override def receive: Actor.Receive =
        ConnectionSuccess | ConnectionFailed

    /**
      * Connection has been established to TCP server.
      */
    private def ConnectionSuccess: Actor.Receive = {
        case c @ Connected(_, _) =>
            // get connection to register itself and write the message on the socket
            val connection = sender()
            connection ! Register(self)
            connection ! Write(message)

            // actor is now the receiver and waits for data
            this.context become (ReceivedResponse | CommandFailed | CloseConnection(connection) | TimeoutFailed)
    }

    /**
      * Data has been received by the tcp server.
      */
    private def ReceivedResponse: Actor.Receive = {
        case Received(response) =>
            // fulfill promise with tcp response
            handleResponse(ReadSuccess(response))
    }

    /**
      * Writing on the socket failed.
      */
    private def CommandFailed: Actor.Receive = {
        case CommandFailed(_) =>
            // return failure
            handleResponse(WriteFailure)
    }

    /**
      * Closing the Connection.
      */
    private def CloseConnection(connection: ActorRef): Actor.Receive = {
        // close command send as message
        case "close"             =>
            // close the connection
            connection ! Close

        // ConnectionClosed command send
        case _: ConnectionClosed =>
            // stop the actor
            stopActor()
    }

    /**
      * Connection to server failed to establish -> stop the actor
      */
    private def ConnectionFailed: Actor.Receive = {
        case a =>
            // stop actor
            stopActor()

            // log event
            error(s"Connection could not be established; host: $host; port: $port")

            // return error message
            handleResponse(ConnectionFailure)
    }

    /**
      * Timeout reading from server -> stop the actor
      */
    private def TimeoutFailed: Actor.Receive = {
        case _ =>
            // stop actor
            stopActor()

            // log event
            error(s"Timeout ($timeout) exceeded; host: $host; port: $port")

            // return error message
            handleResponse(TimeoutFailure)
    }

    /**
      * Stops the actor
      */
    private def stopActor(): Unit =
        this.context stop self

    /**
      * Handles the response of the TCP actor
      *
      * @param tcpResponse The response to handle
      */
    protected def handleResponse(tcpResponse: TCPResponse): Unit
}
