const infect = (map, bursts) => {
  const lines = map.trim().split('\n');

  let x = Math.floor(lines.length / 2);
  let y = x;
  let dirX = 0;
  let dirY = 1;
  let infected = 0;

  const grid = lines
    .reduce(
      (acc, line, y) => [
        ...acc,
        ...line.split('').map((v, x) => ({
          infected: v === '#',
          x,
          y
        }))
      ],
      []
    )
    .filter(item => item.infected)
    .map(item => ({ x: item.x, y: item.y }));

  const isInfected = (x, y) => Boolean(grid.find(item => item.x === x && item.y === y));

  const infect = (x, y) => {
    grid.push({ x, y });
    infected++;
  };

  const clean = (x, y) => {
    const index = grid.findIndex(item => item.x === x && item.y === y);
    grid.splice(index, 1);
  };

  const turn = dir => {
    if (dirX === 0) {
      dirX = dirY * dir;
      dirY = 0;
    } else {
      dirY = -dirX * dir;
      dirX = 0;
    }
  };

  for (let i = 0; i < bursts; i++) {
    const infected = isInfected(x, y);

    if (infected) {
      turn(1);
      clean(x, y);
    } else {
      turn(-1);
      infect(x, y);
    }

    x += dirX;
    y += -dirY;
  }

  return infected;
};

module.exports = infect;
