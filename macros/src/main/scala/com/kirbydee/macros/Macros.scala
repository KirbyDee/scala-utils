package com.kirbydee.macros

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

object Macros {

    def hello = macro helloImpl
    def helloImpl(c: blackbox.Context): c.Expr[Unit] = {
        import c.universe._
        c.Expr(q"""println("hello!")""")
    }

    def hello2(s: String) = macro hello2Impl
    def hello2Impl(c: blackbox.Context)(s: c.Expr[String]): c.Expr[Unit] = {
        import c.universe._

        c.Expr(q"""println("hello " + ${s.tree} + "!")""")
    }

    def getVal(s: Any) = macro getValMacro
    def getValMacro(c: blackbox.Context)(s: c.Expr[Any]) : c.Expr[Any] = {
        import c.universe._

        val q"val $name = $value" = s.tree match {
            case Block(List(valdef), _) => valdef
        }

        c.Expr(q"$value")
    }
}