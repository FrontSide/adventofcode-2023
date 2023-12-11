package advent

object Day6 {

    case class Race(time: Int, currentRecordDistance: Long) {
        val getWinningPossibilitiesCount = (1 to time-1).count{
            holdTime => {
                val newDistance = holdTime.toLong * (time - holdTime)
                //println(s"$holdTime $newDistance")
                newDistance > currentRecordDistance}
            }
    }

    val testRaces = List(Race(7, 9), Race(15, 40), Race(30, 200))

    val finalTestRace = Race(71530, 940200)

    val races = List(Race(53, 275), Race(71, 1181), Race(78, 1215), Race(80, 1524))

    val finalRace = Race(53717880, 275118112151524L)

    def run = {
        //races.map(_.getWinningPossibilitiesCount).product
        finalRace.getWinningPossibilitiesCount
    }


}





