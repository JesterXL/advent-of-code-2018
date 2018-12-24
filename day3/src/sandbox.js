const log = console.log
const fs = require('fs')
const input = fs.readFileSync('./src/claims.txt')
    .toString()
    .split('\n')
    .slice(0, 100)
grid = Object.create(null);

console.log(input.length)
for (const line of input) {
   [num, at, one, two] = line.split(' ');
   [left, top] = one.slice(0, -1).split(',').map(x => Number(x));
   [width, height] = two.split('x').map(x => Number(x));
   for (let x = left; x < left + width; x++) {
    //    log("x:", x)
      for (let y = top; y < top + height; y++) {
          log("y:", y)
         grid[`${x},${y}`] = (grid[`${x},${y}`] || 0) + 1;
      }
   }
}

console.log(Object.values(grid).filter(v => v > 1).length);
// log(grid)