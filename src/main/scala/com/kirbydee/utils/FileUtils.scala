package com.kirbydee.utils

import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO

import com.kirbydee.logr.Logr
import com.kirbydee.utils.Ternary.Implicits._
import com.kirbydee.utils.InputStreamUtils.Implicits._

import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}

object FileUtils {

    /*
     * Type of files.
     */
    object FileType extends Enumeration {
        type FileType = Value
        val FILE = Value
        val DIR  = Value
        val ALL  = Value
    }
    import com.kirbydee.utils.FileUtils.FileType._

    /**
      * Implicit Classes, Conversions etc.
      */
    trait Implicits {

        /**
          * File implicit class Helper
          *
          * @param file The file to manipulate
          */
        implicit class FileHelper(file: File) {

            /**
              * Creates and opens a BufferedReader for the file provided. Will be closed afterwards.
              *
              * @param f The function which tells what to do with the reader
              * @return Unit
              */
            @inline def readFile[A](f: BufferedReader => A): A = {
                // open reader
                val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))

                // use reader
                val a = f(reader)

                // close reader
                reader.close()

                // return a
                a
            }

            /**
              * Creates and opens a BufferedReader for the file provided. Will be closed afterwards.
              *
              * @param f The function for each line in the file
              * @return Unit
              */
            @inline def readFileLine[A](f: String => A): List[A] = {
                // open reader
                val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))

                // use reader to read continually lines, and consume them in the given function
                val aList = (Stream.continually(reader.readLine()).takeWhile(_ != null) map f).toList

                // close reader
                reader.close()

                // return list of a
                aList
            }

            /**
              * Creates and opens a BufferedWriter for the file provided. Will be closed afterwards.
              *
              * @param f The function which tells what to do with the writer
              * @return Unit
              */
            @inline def writeFile(f: BufferedWriter => Unit): Unit =
                writeFile(append = false)(f)

            /**
              * Creates and opens a BufferedWriter for the file provided. Will be closed afterwards.
              *
              * @param append True, if one needs to append text, false otherwise
              * @param f The function which tells what to do with the writer
              * @return Unit
              */
            @inline def writeFile(append: Boolean)(f: BufferedWriter => Unit): Unit = {
                // open writer
                val writer = new BufferedWriter(new FileWriter(file, append))

                // user writer
                f(writer)

                // close writer
                writer.close()
            }

            /**
              * Returns the list of files in the directory.
              * Can be either directories or normal files.
              *
              * @return The list of files in the directory supplied
              */
            @inline def content: List[File] =
                content(ALL)

            /**
              * Returns the list of files in the directory.
              * Can be either directories or normal files.
              *
              * @param fileType The type of the file {FILE, DIR, ALL}
              * @return The list of files in the directory supplied
              */
            @inline def content(fileType: FileType): List[File] = {
                (file.exists() && file.isDirectory) ? {
                    file.listFiles().toList filter { f =>
                        fileType match {
                            case FILE if f.isFile      => true
                            case DIR  if f.isDirectory => true
                            case ALL                   => true
                            case _                     => false
                        }
                    }
                } | {
                    List[File]()
                }
            }

            /**
              * Returns the list of files in the directory.
              * Can be either directories or normal files.
              *
              * @param f The mapping function
              * @return The list of files in the directory supplied
              */
            @inline def withContent[A](f: File => A): List[A] =
                withContent(ALL)(f)

            /**
              * Returns the list of files in the directory.
              * Can be either directories or normal files.
              *
              * @param fileType The type of the file {FILE, DIR, ALL}
              * @param f The mapping function
              * @return The list of files in the directory supplied
              */
            @inline def withContent[A](fileType: FileType)(f: File => A): List[A] =
                content(fileType) map f

            /**
              * Reads an image of the file.
              *
              * @return The BufferedImage
              */
            @inline def img: BufferedImage =
                ImageIO read file

            /**
              * Reads the bytes from the file.
              *
              * @return The byte array
              */
            @inline def bytes: Array[Byte] =
                new FileInputStream(file).bytes
        }

        /**
          * String implicit class Helper
          *
          * @param string The writer
          */
        implicit class StringFileHelper(string: String) {

            /**
              * Returns the file with name given.
              * If not existing, create it
              *
              * @return The file opened/created
              */
            @inline def file: File = {
                val file = new File(string)
                (file.exists && file.isFile)!? file.createNewFile end;
                file
            }

            /**
              * Returns the directory with name given.
              * If not existing, create it
              *
              * @return The directory opened/created
              */
            @inline def dir: File = {
                val directory = new File(string)
                (directory.exists() && directory.isDirectory)!? directory.mkdir end;
                directory
            }

            /**
              * Writes a string into the BufferedWriter
              *
              * @param writer The implicit BufferedWriter
              * @param logr The implicit Logr used to report an error
              */
            @inline def writeFile(implicit writer: BufferedWriter, logr: Logr): Unit = Try {
                writer write string
            }  match {
                case Success(_) => ()
                case Failure(e) => logr.error(s"Could not write text $string: ${e.getMessage}", e)
            }
        }
    }
    object Implicits extends Implicits
}
