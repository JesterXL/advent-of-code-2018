
// getManhattanDistance : Point -> Point -> Int
// getManhattanDistance point1 point2 =
//     let
//         size = 1 -- I have a feeling I may regret this later, but it's a pain passing it down
//         x1 = abs point1.x - point2.x
//         x2 = abs point2.x - point1.x
//         dx = min x1 (size - x2)
//         y1 = abs point1.y - point2.y
//         y2 = abs point2.y - point1.y
//         dy = min y1 (size - y2)
//         distance = abs (dx + dy)
//         dxlog = log "dx" dx
//         dylog = log "dy" dy
//         distancelog = log "distance" distance
//     in
//         distance



const log = console.log

const dist = point1 => point2 => {
    const size = 1
    const x1 = Math.abs(point1.x - point2.x)
    const x2 = Math.abs(point2.x - point1.x)
    const dx = Math.min(x1, (size - x2))
    const y1 = Math.abs(point1.y- point2.y)
    const y2 = Math.abs(point2.y - point1.y)
    const dy = Math.min(y1, (size - y2))
    // log("x1:", x1)
    // log("x2:", x2)
    // log("dx:", dx)
    // log("y1:", y1)
    // log("y2:", y2)
    // log("dy:", dy)
    const distance = Math.abs(dx + dy)
    const distance2 = Math.abs(point2.x-point1.x) + Math.abs(point2.y-point1.y);
    log("distance:", distance, ", distance2:", distance2)
    return distance
}
dist({x: 0, y: 0})({x: 0, y: 0})
dist({x: 0, y: 0})({x: 1, y: 0})
dist({x: 0, y: 0})({x: 0, y: 1})
dist({x: 0, y: 0})({x: 1, y: 1})
dist({x: 233, y: 472})({x: 124, y: 562})
// 684