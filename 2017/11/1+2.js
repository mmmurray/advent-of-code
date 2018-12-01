const directionVectorMap = {
  n: { v: 0, w: 0, y: 1 },
  ne: { v: 1, w: 0, y: 0 },
  se: { v: 0, w: 1, y: 0 },
  s: { v: 0, w: 0, y: -1 },
  sw: { v: -1, w: 0, y: 0 },
  nw: { v: 0, w: -1, y: 0 }
};

const distanceTo = ({ v, w, y }) => {
  const position = { v, w, y };

  while (
    (position.v > 0 && position.y < 0) ||
    (position.v > 0 && position.w < 0) ||
    (position.w < 0 && position.y < 0)
  ) {
    position.v--;
    position.w++;
    position.y++;
  }

  while (
    (position.w > 0 && position.y > 0) ||
    (position.v < 0 && position.y > 0) ||
    (position.v < 0 && position.w > 0)
  ) {
    position.v++;
    position.w--;
    position.y--;
  }

  return Math.abs(position.v) + Math.abs(position.w) + Math.abs(position.y);
};

const nextMax = (position, max) => ({ ...position, max: Math.max(distanceTo(position), max) });

const steps = input => {
  const position = input
    .trim()
    .split(',')
    .map(direction => directionVectorMap[direction])
    .reduce(({ v, w, y, max }, vector) => nextMax({ v: v + vector.v, w: w + vector.w, y: y + vector.y }, max), {
      v: 0,
      w: 0,
      y: 0,
      max: 0
    });

  return { distance: distanceTo(position), max: position.max };
};

module.exports = steps;
