package com.github.rinotc.scalastats

import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.{EitherValues, Inside, OptionValues}

trait BaseFunSpec extends AnyFunSpec with Inside with OptionValues with EitherValues with Diagrams {}
