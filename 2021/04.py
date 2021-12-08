#!/usr/bin/env python3

MARK = "x"

with open("data/04.txt") as f:
    lines = f.readlines()

numbers = [int(x) for x in lines[0].strip().split(",")]
rest = lines[1:]

def wins(board):
    n = len(board)  # assume quadratic
    # rows
    for row in board:
        if all(x == MARK for x in row):
            return True
    # cols
    for i in range(n):
        if all(board[j][i] == MARK for j in range(n)):
            return True
    # main diag
    if all(board[i][i] == MARK for i in range(n)):
        return True
    # off diag
    if all(board[i][n - i - 1] == MARK for i in range(n)):
        return True
    return False

def mark(board, num):
    n = len(board)  # assume quadratic
    for i in range(n):
        for j in range(n):
            if board[i][j] == num:
                board[i][j] = MARK
                return board
    return board

def score(board):
    return sum(
        sum(x for x in row if x != MARK)
        for row in board
    )


# For each bingo board, figure out after how many turns it will win and what
# its winning score will be.
minrounds = len(numbers)
pt1score = 0
maxrounds = 0  # part 2
pt2score = 0

for i in range(0, len(rest), 6):
    board = rest[(i + 1):(i + 6)]
    board = [
        [int(x) for x in line.strip().split()]
        for line in board
    ]
    for i, num in enumerate(numbers):
        board = mark(board, num)
        if wins(board):
            if i < minrounds:
                minrounds = i
                pt1score = score(board) * num
            if i > maxrounds:
                maxrounds = i
                pt2score = score(board) * num
            break

print(pt1score)
print(pt2score)
