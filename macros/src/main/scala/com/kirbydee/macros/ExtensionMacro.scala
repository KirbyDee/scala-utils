package com.kirbydee.macros

import com.kirbydee.utils.Extension
import org.reflections.Reflections

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

object ExtensionMacro {

    /**
      * Gets the possible Extension from a String.
      *
      * @param extension The string extension
      * @return The possible Extension in com.logograb.utils.Extension
      */
    def from(extension: String): Option[Extension] = macro fromImpl
    def fromImpl(c: blackbox.Context)(extension: c.Expr[String]): c.Expr[Option[Extension]] = {
        import c.universe._

        // get all different extensions in "com.logograb.utils" and parse them to their name
        import scala.collection.JavaConversions._
        val reflect = new Reflections("com.logograb.utils")
        val allExtensions = reflect.getSubTypesOf(classOf[Extension]).toList.map(_.getSimpleName.replace("$", ""))

        // create match cases for each extension
        // ->   case $e: AsString => Some($e: AsObject)
        val cases = allExtensions map { e =>
            cq"$e => Some(${Ident(TermName(e))})"
        }

        // return the right extension
        c.Expr(q"""
                ${extension.tree}.toUpperCase match {
                    case ..$cases
                    case _ => None
                }
            """)
    }
}