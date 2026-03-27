package it.unibo.pps.tasks.adts

import org.junit.Test
import it.unibo.pps.u03.extensionmethods.Sequences.Sequence
import Sequence.*
import it.unibo.pps.tasks.adts.SchoolModel.BasicSchoolModule.{course, emptySchool}
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

class SchoolModelTest:
  import SchoolModel.BasicSchoolModule.*

  private val teacher1: BasicTeacher = teacher("John")
  private val teacher2: BasicTeacher = teacher("Bob")
  private val course1 = course("Math")
  private val course2 = course("Science")
  private val course3 = course("History")
  private val school = emptySchool
    .setTeacherToCourse(teacher1, course1)
    .setTeacherToCourse(teacher1, course2)
    .setTeacherToCourse(teacher1, course3)

  @Test def testCoursesString(): Unit =
    assertEquals(Cons("Math", Cons("Science", Cons("History", Nil()))), school.courses)
    assertEquals(Nil(), emptySchool.courses)

  @Test def testTeachersString(): Unit =
    assertEquals(Cons("John", Nil()), school.teachers)
    assertEquals(Nil(), emptySchool.teachers)

  @Test def testCoursesOfATeacher(): Unit =
    assertEquals(Cons(course1, Cons(course2, Cons(course3, Nil()))), school.coursesOfATeacher(teacher1))
    assertEquals(Nil(), school.coursesOfATeacher(teacher2))

  @Test def testHasTeacher(): Unit =
    assertTrue(school.hasTeacher("John"))
    assertFalse(school.hasTeacher("Bob"))

  @Test def testHasCourse(): Unit =
    assertTrue(school.hasCourse("Math"))
    assertFalse(school.hasCourse("Chemistry"))