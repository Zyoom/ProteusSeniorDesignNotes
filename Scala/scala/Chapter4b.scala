package Chapter4;
  case class Person(name: String, age: Int)
  // defined a case class Person
  val david: Person = Person("David", 20)

  def greeting(person: Person): String = s"Hi ${person.name}"

