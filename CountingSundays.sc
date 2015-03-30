import org.joda.time._

val d1 = new DateTime(1901, 1, 1, 0, 0)
val d2 = new DateTime(2000, 12, 31, 0, 0)

val days = for {
  year <- 1901 to 2000
  mounth <- 1 to 12
  day <- 1 to (if (mounth == 9 || mounth == 4 || mounth == 6 || mounth == 11) 30
  else if (mounth == 2)
    if (((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)) 29 else 28
  else 31)

} yield (year, mounth, day)

val (idx, day) = (2 to 56524 zip days).filter({ case (x, y) => x % 7 == 0}) unzip
val sundays = day.filter({ case (y, m, d) => d == 1})
sundays.size
/*
val localdays = days.map({ case (y, m, d) => new LocalDate(y, m, d)})
val ff = (0 to 56524).map(x => new LocalDate(1901, 1, 1).plusDays(x))
/*ff zip localdays filter {
  case (x, y) =>! x.equals(y)
}*/
val d4 = new LocalDate(2000,2,28)
d4.plusDays(1)
localdays(36217)
localdays(36218)
/*val dd = localdays zip localdays.tail map { case (x, y) => Days.daysBetween(y, x).getDays}
dd.indexOf(-2)

localdays(36217)
localdays(36218)*/
//sundays.map({case(y,m,d)=>new LocalDate(y,m,d).dayOfWeek().get()}).indexOf(2)
//new LocalDate(1901-01-01)


*/
day.groupBy({ case (y, m, d) => new LocalDate(y, m, d).getDayOfWeek})
new LocalDate(1901,1,8).getDayOfWeek