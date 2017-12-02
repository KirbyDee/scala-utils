package com.kirbydee.macros

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

sealed trait Component {
    def toTree(implicit c: blackbox.Context): c.Tree

    def derive: Component
}

case class Negate(value: Component) extends Component {
    override def toTree(implicit c: blackbox.Context): c.Tree = {
        import c.universe._
        q"-${value.toTree}"
    }

    override def derive: Component = Negate(value.derive)
}

case class Multiply(first: Component, second: Component) extends Component {
    override def toTree(implicit c: blackbox.Context): c.Tree = {
        import c.universe._
        q"${first.toTree} * ${second.toTree}"
    }

    override def derive: Component = Add(Multiply(first, second.derive), Multiply(first.derive, second))
}

case class Add(first: Component, second: Component) extends Component {
    override def toTree(implicit c: blackbox.Context): c.Tree = {
        import c.universe._
        q"${first.toTree} + ${second.toTree}"
    }

    override def derive: Component = Add(first.derive, second.derive)
}

case class Power(first: Component, second: Component) extends Component {
    override def toTree(implicit c: blackbox.Context): c.Tree = {
        import c.universe._
        q"Math.pow(${first.toTree}, ${second.toTree})"
    }

    override def derive: Component = this match {
        case Power(Variable(_), DoubleConstant(b)) => Multiply(second, Power(first, DoubleConstant(b - 1)))
    }
}

case class Variable(name: String) extends Component {
    override def toTree(implicit c: blackbox.Context): c.Tree = {
        import c.universe._
        Ident(TermName(name))
    }

    override def derive: Component = DoubleConstant(1)
}

case class DoubleConstant(value: Double) extends Component {
    override def toTree(implicit c: blackbox.Context): c.Tree = {
        import c.universe._
        Literal(Constant(value))
    }

    override def derive: Component = DoubleConstant(0)
}

object Derivative {
    def getComponent(tree: Trees#Tree)(implicit c: blackbox.Context): Component = {
        import c.universe._
        tree match {
            case Ident(TermName(x)) => Variable(x)
            case Literal(Constant(a)) => DoubleConstant(a.toString.toDouble)
            case q"-$x" => Negate(getComponent(x))
            case q"+$x" => getComponent(x)
            case q"$a + $b" => Add(getComponent(a), getComponent(b))
            case q"$a - $b" => Add(getComponent(a), Negate(getComponent(b)))
            case q"$a * $b" => Multiply(getComponent(a), getComponent(b))
            case q"java.this.lang.Math.pow($a, $b)" => Power(getComponent(a), getComponent(b))
        }
    }

    def extractComponents(tree: Trees#Tree)(implicit c: blackbox.Context): List[Component] = {
        import c.universe._
        tree match {
            case q"$nextTree + $arg" =>
                getComponent(arg) :: extractComponents(nextTree)
            case q"$nextTree - $arg" =>
                Negate(getComponent(arg)) :: extractComponents(nextTree)
            case somethingElse => getComponent(somethingElse) :: Nil
        }
    }

    def derivative(f: Double => Double): Double => Double = macro derivativeImpl

    def derivativeImpl(c: blackbox.Context)(f: c.Expr[Double => Double]): c.Expr[Double => Double] = {
        import c.universe._

        // Function(Arguments, Body)
        // Function(List[ValDef], Tree)
        // val q"($param) => $funcBody" = f.tree
        // val ValDef(_, name, _, _) = param
        val Function(List(ValDef(_, name, _, _)), funcBody) = f.tree

        val components = extractComponents(funcBody)(c)
        val transformedComponents = components.map(_.derive.toTree(c)).reduce((a, b) => q"$a + $b")

        c.Expr(q"($name: Double) => $transformedComponents")
    }
}