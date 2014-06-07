package org.agh.view

import java.awt.Color
import java.awt.Color._
import org.agh._

trait Seeds {
  def seeds(ns: Int): Seq[Color] = {
    var s : Seq[Color] = Seq.empty

    def draw: Color = {
      randomColor
    }

    s
  }
}
