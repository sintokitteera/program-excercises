#!/bin/python3
import re

first_mult_input =input().rstrip().split()
n= int(first_mult_input[0])
m= int(first_mult_input[1])
matrix =[]

for _ in range(n):
    matrix_item = input()
    matrix.append(matrix_item)
print(matrix)

matrix_parsed=""
for i in range(m):
    for j in range(n):
        matrix_parsed += matrix[j][i]
print(f'Decoded Matrix: {matrix_parsed}')

result = re.sub(r"(?<=[0-9A-Za-z])[ !@#$%&]+(?=[0-9A-Za-z])"," ",matrix_parsed)
print(f'Result: {result}')
