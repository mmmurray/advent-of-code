const parsePosition = p => p.split(',').map(Number);
const parseMatches = ([_, p, v, a]) => ({ p: parsePosition(p), v: parsePosition(v), a: parsePosition(a) });
const parseLine = line => parseMatches(/p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>/.exec(line));
const add = (v1, v2) => [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]];
const equals = (v1, v2) => v1[0] === v2[0] && v1[1] === v2[1] && v1[2] === v2[2];

const simulate = input => {
  const particles = input
    .trim()
    .split('\n')
    .map(parseLine);

  let iteration = 0;

  while (iteration++ < 100) {
    particles.forEach((particle, index) => {
      particle.v = add(particle.v, particle.a);
      particle.p = add(particle.p, particle.v);
    });

    const collisions = [];
    particles.forEach((particle, index) => {
      const collider = particles.find(p2 => p2 !== particle && equals(p2.p, particle.p));
      if (collider) {
        collisions.push(index);
      }
    });

    collisions.reverse();

    collisions.forEach(collision => {
      particles.splice(collision, 1);
    });
  }

  return particles.length;
};

module.exports = simulate;
