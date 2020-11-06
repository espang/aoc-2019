use "files"

actor Main
  let _env : Env
  new create(env: Env) =>
    _env = env
    try
      let auth = env.root as AmbientAuth
      let file = recover File(FilePath(auth, "input.txt")?) end
      let part1 = Part1(env, consume file)
      part1.calculate(this)
      
      let file2 = recover File(FilePath(auth, "input.txt")?) end
      let part2 = Part1(env, consume file2)
      part2.calculate_p2(this)
    else
      env.out.print("hmmm")
    end

  be display(s: String) =>
    _env.out.print("result: " + s)

actor Part1
  let _file: File
  let _env: Env

  new create(env: Env, file: File iso) =>
    _file = consume file
    _env = env

  fun rmass(i: I64): I64 =>
    if i <= 0 then
      0
    else
      let m = (i / 3) - 2
      m + rmass(m)
    end

  be calculate_p2(main: Main) =>
    var v: I64 = 0
    for line in _file.lines() do
      try
        let nbr = line.i64()?
        v = v + rmass(nbr)
      end
    end
    main.display(v.string())

  fun mass(i: I64): I64 =>
    (i / 3) - 2

  be calculate(main: Main) =>
    var v: I64 = 0
    for line in _file.lines() do
      try
        let nbr = line.i64()?
        v = v + mass(nbr)
      end
    end
    main.display(v.string())