namespace AdventOfCode.Y2021

open Helpers

module Day17 =

  let isInside ((xMin, yMin), (xMax, yMax)) (x,y) =
    (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)

  let hasMissed ((xMin, yMin), (xMax, yMax)) (x,y) =
    x > xMax || y < yMin

  let passesThrough target trajectory =
    trajectory |> List.fold (fun hasHit position -> isInside target position || hasHit) false

  type Outcome = Hit | Missed

  let rec calculateTrajectory target (x, y) (vx, vy) =

    let x, y = (x + vx), (y + vy)
    let vx = vx + if vx > 0 then -1 elif vx < 0 then 1 else 0
    let vy = vy - 1
    //ps "Position: " (x,y)
    if isInside target (x,y) then Hit
    elif hasMissed target (x,y) then Missed
    else calculateTrajectory target (x, y) (vx, vy)


    //let (xVel, yVel) = xVel - 1, yVel - 1

  let target = ((265, -103),(287, -58)) // max y speed is 102, max x speed 23
  //let target = ((20, -10), (30, -5))


  module Part1 =
    let go () =
      // found iteratively by hand, noticing that projectile always returns through y position 0
      // took three of four tries to get the x right, y was easy
      // naughty, but done
      calculateTrajectory target (0, 0) (23, 102)


  module Part2 =
    let go () =
      let vys = [-105 .. 105] // naughty to have done it by hand, I know
      let vxs = [1 .. 288] // naughty to have done it by hand, I know

      let velocities = vys |> List.collect (fun vy -> vxs |> List.map (fun vx -> vx, vy))  |> List.sort
      ps "Number of velocities: " (List.length velocities)

      let hitCount =
        velocities
        |> List.map (calculateTrajectory target (0,0))
        |> pso "Outcomes: "
        |> List.countBy ((=) Hit)

      hitCount

      // need to generate a list of all reasonable starting velocities



  let run () =
    Part1.go () |> ps "Part 1: "
    Part2.go () |> ps "Part 2: "

(*
Position: (21, 27)
Position: (21, 19)
Position: (21, 10)
Position: (21, 0)
Position: (21, -11)

Position: (21, 30)
Position: (21, 21)
Position: (21, 11)
Position: (21, 0)
Position: (21, -12)

Position: (21, 42)
Position: (21, 33)
Position: (21, 23)
Position: (21, 12)
Position: (21, 0)
Position: (21, -13)




*)
