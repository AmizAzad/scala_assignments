Assume students are appearing for an exam that is starting in M hours.
Each student gets a different exam paper, and it could start at a different time from others.
The exam will contain questions from K lessons. Each lesson takes L hours to learn.
You are given a CSV file containing many lines, each having the integers K, L, M as defined above.

Write a scala snippet that reads this file and for each line produces an output:
"YES" if the student can finish preparation BEFORE the exam begins.
"NO" if the student will not be able to finish preparation before the exam begins.

Pay attention to the amount of time needed by the program to execute - try to be as optimal as possible.
To run this program, provide the path to exam_data.csv file (exam_data_problem/exam_data.csv) in command line argument.