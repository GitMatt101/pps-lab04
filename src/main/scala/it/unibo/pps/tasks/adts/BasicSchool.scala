package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.extensionmethods.Sequences.Sequence

case class BasicSchool(
  private val teachers: Sequence[BasicTeacher],
  private val courses: Sequence[BasicCourse],
  private val teacherToCourses: Sequence[(BasicTeacher, BasicCourse)]
)
