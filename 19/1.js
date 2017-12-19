const validPattern = /\||\-|[A-Z]|\+/;

const follow = map => {
  const lines = map.split('\n').map(l => l.split(''));
  const position = { x: lines[0].indexOf('|'), y: 0 };

  let direction = 3;
  let steps = 0;
  let letters = '';

  const addLetter = current => {
    if (/[A-Z]/.test(current)) {
      letters += current;
    }
  };

  while (true) {
    addLetter(lines[position.y][position.x]);
    steps++;

    if (direction === 1 || direction === 3) {
      const sign = direction === 1 ? -1 : 1;
      const next = lines[position.y + sign][position.x];

      if (validPattern.test(next)) {
        position.y += sign;
      } else {
        break;
      }

      if (next === '+') {
        const left = lines[position.y][position.x - 1];
        const right = lines[position.y][position.x + 1];

        if (validPattern.test(left)) {
          direction = 4;
        } else if (validPattern.test(right)) {
          direction = 2;
        }
      }
    } else if (direction === 2 || direction === 4) {
      const sign = direction === 4 ? -1 : 1;
      const next = lines[position.y][position.x + sign];

      if (validPattern.test(next)) {
        position.x += sign;
      } else {
        break;
      }

      if (next === '+') {
        const up = lines[position.y - 1][position.x];
        const down = lines[position.y + 1][position.x];

        if (validPattern.test(up)) {
          direction = 1;
        } else if (validPattern.test(down)) {
          direction = 3;
        }
      }
    }
  }

  return { letters, steps };
};

module.exports = follow;
