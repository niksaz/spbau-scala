package ru.spbau.sazanovich.nikita.collect.immutable

class TestParent(val name: String) {

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: TestParent => this.name == that.name
      case _ => false
    }
  }
}
