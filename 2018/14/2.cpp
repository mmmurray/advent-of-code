#include <iostream>

struct Node {
  int value;
  Node *next;
  Node *prev;
};

Node* createNode(int _value) {
  Node* node = new Node();
  node->value = _value;
  node->next = nullptr;
  node->prev = nullptr;
  return node;
}

class List {
  public:
    int length;
    Node* head;
    Node* tail;

    List(int value) {
      length = 1;
      head = createNode(value);
      tail = head;
      tail->next = head;
      head->prev = tail;
    };

    Node* append(int value) {
      length++;
      Node* newNode = createNode(value);
      tail->next = newNode;
      newNode->prev = tail;
      tail = newNode;
      tail->next = head;
      return newNode;
    }

    Node* insertAfter(Node* node, int value) {
      if (node == tail) {
        return append(value);
      }

      length++;
      Node* newNode = createNode(value);
      newNode->next = node->next;
      node->next = newNode;
      newNode->prev = node;

      return newNode;
    }

    bool isComplete() {
      Node* current = tail;
      if (current->value != 1) return false;
      current = current->prev;
      if (current->value != 0) return false;
      current = current->prev;
      if (current->value != 8) return false;
      current = current->prev;
      if (current->value != 7) return false;
      current = current->prev;
      if (current->value != 4) return false;
      current = current->prev;
      if (current->value != 0) return false;

      return true;
    }
};

class Elf {
  public:
    Node* current;

    Elf(Node* _current) {
      current = _current;
    }

    int value() {
      return current->value;
    }

    void move() {
      int steps = 1 + current->value;
      for (int i = 0; i < steps; i++) {
        current = current->next;
      }
    }
};

bool tick(List& list, Elf& elf1, Elf& elf2) {
  int sum = elf1.value() + elf2.value();

  if (sum < 10) {
    list.append(sum);
    if (list.isComplete()) return true;
  } else {
    list.append(1);
    if (list.isComplete()) return true;
    list.append(sum - 10);
    if (list.isComplete()) return true;
  }

  elf1.move();
  elf2.move();

  return false;
}

int main() {
  List list(3);
  Elf elf1(list.head);
  Elf elf2(list.insertAfter(list.head, 7));

  bool complete = false;

  while (!complete) {
    complete = tick(list, elf1, elf2);
  }

  std::cout << (list.length - 6) << std::endl;

  return 0;
}
