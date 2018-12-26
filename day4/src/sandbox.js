
const a = [0, 1, 2, 3, 4, 5, 6]
const total = 3
const start = 2
const end = start + total

for(let i = 0; i<a.length; i++) {
    const wat = i >= start && i <= end
    console.log("i:", i, " wat:", wat)
}