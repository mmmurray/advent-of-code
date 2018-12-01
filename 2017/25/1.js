let position = 0;
let state = 'A';
const positiveTape = [];
const negativeTape = [];

const getValue = () => (position < 0 ? negativeTape[-position] : positiveTape[position]) || 0;

const setValue = value => {
  if (position < 0) {
    negativeTape[-position] = value;
  } else {
    positiveTape[position] = value;
  }
};

for (let step = 0; step < 12261543; step++) {
  const value = getValue();

  switch (state) {
    case 'A': {
      if (value === 0) {
        setValue(1);
        position++;
        state = 'B';
      } else {
        setValue(0);
        position--;
        state = 'C';
      }
      break;
    }
    case 'B': {
      if (value === 0) {
        setValue(1);
        position--;
        state = 'A';
      } else {
        setValue(1);
        position++;
        state = 'C';
      }
      break;
    }
    case 'C': {
      if (value === 0) {
        setValue(1);
        position++;
        state = 'A';
      } else {
        setValue(0);
        position--;
        state = 'D';
      }
      break;
    }
    case 'D': {
      if (value === 0) {
        setValue(1);
        position--;
        state = 'E';
      } else {
        setValue(1);
        position--;
        state = 'C';
      }
      break;
    }
    case 'E': {
      if (value === 0) {
        setValue(1);
        position++;
        state = 'F';
      } else {
        setValue(1);
        position++;
        state = 'A';
      }
      break;
    }
    case 'F': {
      if (value === 0) {
        setValue(1);
        position++;
        state = 'A';
      } else {
        setValue(1);
        position++;
        state = 'E';
      }
      break;
    }
  }
}

const count = positiveTape.filter(x => x === 1).length + negativeTape.filter(x => x === 1).length;

console.log(count);
