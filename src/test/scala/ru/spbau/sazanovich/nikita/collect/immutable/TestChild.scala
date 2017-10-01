package ru.spbau.sazanovich.nikita.collect.immutable

case class TestChild(override val name: String, id: Int) extends TestParent(name)
