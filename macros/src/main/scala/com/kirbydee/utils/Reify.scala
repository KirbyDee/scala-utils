package com.kirbydee.utils


object Reify {

    /**
      * Creates a AST of the expression provided (useful for creating macros).
      *
      * @param expr The expression to transform
      * @tparam T The type of Expression
      * @return The AST of the expression
      */
    def apply[T](expr: T): _root_.scala.reflect.runtime.universe.Expr[T] =
        reflect.runtime.universe.reify(expr)
}
