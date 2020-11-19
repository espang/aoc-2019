import java.io.File

fun fileToMemory(filename: String): List<Int> =
    File(filename)
        .readText(Charsets.UTF_8)
        .split(",")
        .map { it.trim().toInt() }

fun executeInstructions(memory: MutableList<Int>, index: Int = 0) {
    val operation = memory[index]
    when (operation) {
        1 ->
            {
                memory[memory[index + 3]] = memory[memory[index + 1]] + memory[memory[index + 2]]
                executeInstructions(memory, index + 4)
            }
        2 ->
            {
                memory[memory[index + 3]] = memory[memory[index + 1]] * memory[memory[index + 2]]
                executeInstructions(memory, index + 4)
            }
        99 -> Unit
        else ->
            println("unexpected operation $operation")
    }
}

data class Result(val noun: Int, val verb: Int)

fun resultToString(r: Result): String {
    val (noun, verb) = r
    return "${noun * 100 + verb}"
}

fun searchFor(memory: List<Int>, wanted: Int): Result {
    for (noun in 0..99) {
        for (verb in 0..99) {
            var mem = memory.toMutableList()
            mem[1] = noun
            mem[2] = verb
            executeInstructions(mem)
            if (mem[0] == wanted) return Result(noun, verb)
        }
    }
    // This should be an exception or it should return either a Result or Nothing
    return Result(-1, -1)
}

fun main(args: Array<String>) {
    val mem = fileToMemory("day2-input.txt")
    var memory = mem.toMutableList()
    memory[1] = 12
    memory[2] = 2
    executeInstructions(memory)
    println("part1: ${memory[0]}")
    val result = searchFor(mem, 19690720)
    println("part2: ${resultToString(result)}")
}
