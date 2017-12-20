const parsePosition = p => p.split(',').map(Number);
const parseMatches = ([_, p, v, a]) => ({ p: parsePosition(p), v: parsePosition(v), a: parsePosition(a) });
const parseLine = line => parseMatches(/p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>/.exec(line));
const add = (v1, v2) => [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]];

const simulate = input => {
  const particles = input
    .trim()
    .split('\n')
    .map(parseLine);

  let iteration = 0;
  let closestIndex = null;
  while (iteration++ < 400) {
    let closestDistance = Infinity;

    particles.forEach((particle, index) => {
      particle.v = add(particle.v, particle.a);
      particle.p = add(particle.p, particle.v);

      const distance = Math.abs(particle.p[0]) + Math.abs(particle.p[1]) + Math.abs(particle.p[2]);
      if (distance < closestDistance) {
        closestDistance = distance;
        closestIndex = index;
      }
    });
  }

  return closestIndex;
};

module.exports = simulate;
