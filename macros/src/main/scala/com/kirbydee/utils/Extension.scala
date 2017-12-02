package com.kirbydee.utils

trait Extension {

    override def toString: String =
        s".$toStringWithoutDot"

    def toStringWithoutDot: String =
      getClass.getSimpleName.replace("$", "").toLowerCase
}

/*
 * Image extensions
 */
case object JPG  extends Extension
case object JPEG extends Extension
case object PNG  extends Extension

/*
 * Video extensions
 */
case object AVI   extends Extension
case object OGG   extends Extension
case object GIF   extends Extension
case object M4V   extends Extension
case object MPEG  extends Extension
case object MPG   extends Extension
case object MPE   extends Extension
case object MP4   extends Extension
case object WEBM  extends Extension

/*
 * Other extensions
 */
case object JSON extends Extension