package com.github.rinotc.scalamath

import org.scalatest.compatible.Assertion
import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.{EitherValues, Inside, OptionValues}
import org.scalatest.matchers.should.Matchers

trait BaseFunSpec extends AnyFunSpec with Matchers with Inside with OptionValues with EitherValues with Diagrams {

  def assertTrue(condition: Boolean): Assertion = assert(condition)

  def assertTrue(condition: Boolean, clue: Any): Assertion = assert(condition, clue)

  def assertFalse(condition: Boolean): Assertion = assert(!condition)

  def assertFalse(condition: Boolean, clue: Any): Assertion = assert(!condition, clue)
}
