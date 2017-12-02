package com.kirbydee.utils

import java.lang.annotation.Annotation
import java.lang.reflect.{AnnotatedElement, Field, Method, Parameter}

import scala.language.postfixOps
import scala.util.Try

object Reflect {

    // type aliases
    import Alias._
    object Alias {
        type ReflectedParameter = Parameter
        type ReflectedField = Field
        type ReflectedMethod = Method
    }

    def toClass(classPath: String): Try[Class[_]] = Try {
        Class forName classPath
    }

    def hasAnnotation[A <: Annotation](classPath: String)(annotationClass: Class[A]): Try[Boolean] =
        toClass(classPath) flatMap(hasAnnotation(_)(annotationClass))

    def hasAnnotation[A <: Annotation](elem: AnnotatedElement)(annotationClass: Class[A]): Try[Boolean] =
        getAnnotation(elem)(annotationClass) map(_.isDefined)

    def getAnnotation[A <: Annotation](classPath: String)(annotationClass: Class[A]): Try[Option[A]] =
        toClass(classPath) flatMap(getAnnotation(_)(annotationClass))

    def getAnnotation[A <: Annotation](elem: AnnotatedElement)(annotationClass: Class[A]): Try[Option[A]] = Try {
        Option(elem getAnnotation annotationClass)
    }

    def getParameters(clazz: Class[_]): List[ReflectedParameter] =
        clazz.getConstructors.headOption.map(_.getParameters toList) getOrElse Nil

    def getFields(clazz: Class[_]): List[ReflectedField] =
        clazz.getDeclaredFields toList

    def getField(classPath: String)(fieldName: String): Try[ReflectedField] =
        toClass(classPath) flatMap(getField(_)(fieldName))

    def getField(clazz: Class[_])(fieldName: String): Try[ReflectedField] = Try {
        clazz getDeclaredField fieldName
    }

    def getFieldValue(fieldName: String)(any: Any): Try[Any] =
        getField(any getClass)(fieldName) flatMap(getFieldValue(_)(any))

    def getFieldValue(field: ReflectedField)(any: Any): Try[Any] = Try {
        field setAccessible true
        field get any
    }

    def getMethods(clazz: Class[_]): List[ReflectedMethod] =
        clazz.getMethods toList

    def getMethod(classPath: String)(methodName: String): Try[ReflectedMethod] =
        toClass(classPath) flatMap(getMethod(_)(methodName))

    def getMethod(clazz: Class[_])(methodName: String): Try[ReflectedMethod] = Try {
        clazz getMethod methodName
    }

    def getMethodWithArgs(classPath: String)(methodName: String)(parameterTypes: Class[_]*): Try[ReflectedMethod] =
        toClass(classPath) flatMap(getMethodWithArgs(_)(methodName)(parameterTypes: _*))

    def getMethodWithArgs(clazz: Class[_])(methodName: String)(parameterTypes: Class[_]*): Try[ReflectedMethod] = Try {
        clazz.getMethod(methodName, parameterTypes: _*)
    }

    def getMethodValue(methodName: String)(any: Any): Try[Any] =
        getMethod(any getClass)(methodName) flatMap(getMethodValue(_)(any))

    def getMethodValue(method: ReflectedMethod)(any: Any): Try[Any] = Try {
        method setAccessible true
        method invoke any
    }

    def getMethodValueWithArgs(methodName: String)(any: Any, args: Any*): Try[Any] =
        getMethodWithArgs(any getClass)(methodName)(args.map(_.getClass): _*) flatMap(getMethodValueWithArgs(_)(any, args: _*))

    def getMethodValueWithArgs(method: ReflectedMethod)(any: Any, args: Any*): Try[Any] = Try {
        method setAccessible true
        method.invoke(any, args.map(_.asInstanceOf[Object]): _*)
    }

    def newInstance(classPath: String): Try[Any] =
        toClass(classPath) flatMap newInstance

    def newInstance(clazz: Class[_]): Try[Any] = Try {
        clazz.newInstance()
    }

    def newInstanceWithConstructor(classPath: String)(args: Any*): Try[Any] =
        toClass(classPath) flatMap(newInstanceWithConstructor(_)(args))

    def newInstanceWithConstructor(clazz: Class[_])(args: Any*): Try[Any] = Try {
        val constructor = clazz.getConstructors()(0)
        val workArg: Array[AnyRef] = new Array(args.length)
        var i = 0
        for (arg <- args) {
            workArg(i) = arg match {
                case i: AnyRef  => i
                case i: Int     => new java.lang.Integer(i)
                case i: Long    => new java.lang.Long(i)
                case i: Float   => new java.lang.Float(i)
                case i: Double  => new java.lang.Double(i)
                case i: Boolean => new java.lang.Boolean(i)
                case i: Char    => new java.lang.Character(i)
                case i: Byte    => new java.lang.Byte(i)
                case i: Short   => new java.lang.Short(i)
                case _          => arg.asInstanceOf[AnyRef]
            }
            i += 1
        }
        constructor.newInstance(workArg: _*)
    }

    def methodHasReturnType[T](classPath: String)(methodName: String)(returnClass: Class[T]): Try[Boolean] =
        toClass(classPath) flatMap(methodHasReturnType(_)(methodName)(returnClass))

    def methodHasReturnType[T](clazz: Class[_])(methodName: String)(returnClass: Class[T]): Try[Boolean] =
        getMethod(clazz)(methodName) flatMap(methodHasReturnType(_)(returnClass))

    def methodHasReturnType[T](method: Method)(returnClass: Class[T]): Try[Boolean] = Try {
        method.getReturnType equals returnClass
    }
}
