const travel = target => {
  let x = 0;
  let y = 0;
  let dirX = 1;
  let dirY = 0;
  let remainingSteps = 1;
  let nextSteps = 1;

  const turn = () => {
    if (dirX === 0) {
      nextSteps += 1;
      dirX = -dirY;
      dirY = 0;
    } else if (dirY === 0) {
      dirY = dirX;
      dirX = 0;
    }
    remainingSteps = nextSteps;
  };

  const visited = [{ x: 0, y: 0, c: 1 }];

  const countNeighbours = (x, y) => {
    let total = 0;
    visited.forEach(v => {
      if (Math.abs(v.x - x) <= 1 && Math.abs(v.y - y) <= 1) {
        total += v.c;
      }
    });
    return total;
  };

  while (true) {
    x += dirX;
    y += dirY;

    const neighbourCount = countNeighbours(x, y);
    visited.push({ x, y, c: neighbourCount });

    if (neighbourCount > target) {
      return neighbourCount;
    }

    remainingSteps -= 1;
    if (remainingSteps === 0) {
      turn();
    }
  }
};

console.log(travel(368078));
