package com.github.rinotc.scalamath

import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.{EitherValues, Inside, OptionValues}
import org.scalatest.matchers.should.Matchers

trait BaseFunSpec extends AnyFunSpec with Matchers with Inside with OptionValues with EitherValues with Diagrams
