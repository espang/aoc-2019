import java.io.File

fun fuel_for(mass: Int): Int =
    mass / 3 - 2

fun cumulative_fuel_for(mass: Int, fuel: Int = 0): Int {
    val fuel_needed = fuel_for(mass)
    return if (fuel_needed <= 0) {
        fuel
    } else {
        cumulative_fuel_for(
            fuel_needed,
            fuel + fuel_needed,
        )
    }
}

fun main(args: Array<String>) {
    var file = File("day1-input.txt")
    var total = 0
    var total_part2 = 0
    file.forEachLine {
        total += fuel_for(it.toInt())
        total_part2 += cumulative_fuel_for(mass = it.toInt())
    }
    println("total part1: $total")
    println("total part2: $total_part2")
}
