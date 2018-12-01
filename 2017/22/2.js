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
          state: v === '#' ? '#' : '.',
          x,
          y
        }))
      ],
      []
    )
    .filter(item => item.state === '#');

  const getState = (x, y) => {
    const item = grid.find(item => item.x === x && item.y === y);
    return item ? item.state : '.';
  };

  const infect = (x, y) => {
    grid.push({ x, y, state: '#' });
    infected++;
  };

  const mark = (x, y, state) => {
    let item = grid.find(item => item.x === x && item.y === y);
    if (item) {
      item.state = state;
    } else {
      item = { x, y, state };
      grid.push(item);
    }
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
    const state = getState(x, y);

    if (state === '.') {
      turn(-1);
      mark(x, y, 'W');
    } else if (state === 'W') {
      mark(x, y, '#');
      infected++;
    } else if (state === '#') {
      turn(1);
      mark(x, y, 'F');
    } else if (state === 'F') {
      dirX = -dirX;
      dirY = -dirY;
      mark(x, y, '.');
    }

    x += dirX;
    y += -dirY;
  }

  return infected;
};

module.exports = infect;
