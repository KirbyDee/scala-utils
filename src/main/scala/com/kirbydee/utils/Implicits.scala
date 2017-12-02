package com.kirbydee.utils

object Implicits extends

        // utils Implicits
        StringUtils.Implicits
        with JsUtils.Implicits
        with ListUtils.Implicits
        with FileUtils.Implicits
        with OptionUtils.Implicits
        with FutureUtils.Implicits
        with HttpUtils.Implicits
        with InputStreamUtils.Implicits
        with PartialFunctionUtils.Implicits
        with BufferedImageUtils.Implicits
        with ExtensionUtils.Implicits

        // extractor Implicits
        with Extractor.Implicits

        // predicate Implicits
        with Predicate.Implicits

        // regex Implicits
        with Regex.Implicits

        // ternary Implicits
        with Ternary.Implicits

        // time Implicits
        with Time.Implicits

        // as-is Implicits
        with AsIs.Implicits

        // asString Implicits
        with ToString.Implicits