const without = (part, parts) => parts.filter(p => p !== part);

const build = input => {
  const parts = input
    .trim()
    .split('\n')
    .map(l => l.split('/').map(Number));

  let strongest = 0;
  let longest = 0;
  let strongestLongest = 0;

  const recurse = (connector, remainingParts, totalStrength, length) => {
    const candidateParts = remainingParts.filter(part => part[0] === connector || part[1] === connector);

    candidateParts.forEach(candidatePart => {
      const nextConnector = candidatePart[0] === connector ? candidatePart[1] : candidatePart[0];
      const nextStrength = totalStrength + candidatePart[0] + candidatePart[1];
      if (nextStrength > strongest) {
        strongest = nextStrength;
      }
      if (length >= longest) {
        longest = length;
        if (nextStrength > strongestLongest) {
          strongestLongest = nextStrength;
        }
      }
      recurse(nextConnector, without(candidatePart, remainingParts), nextStrength, length + 1);
    });
  };

  recurse(0, parts, 0, 0);

  return { strongest, strongestLongest };
};

module.exports = build;
