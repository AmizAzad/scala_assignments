A thief wants to cross a hallway of N doors. These N doors are represented as a string. Each door is initially either open or close, represented by 1 (for open) or 0 (for closed) respectively. The thief is required to go through all the doors one by one in the order that they appear, starting from the leftmost door and moving only rightwards at each step.

To make the journey easier, the thief has a remote control, using which he can flip the state of all the doors at once. i.e. whenever the remote is used, all open doors will close and all the closed doors will open at the same time. Determine the minimum number of times the thief has to use this remote to cross the hallway of doors.

For example, the doors are 10011

The thief will start from the leftmost door and enter the first door. After passing through that door, the remote is used. This flips the string to 01100.

Now the thief passes through the next two doors in one go (because they are both open). Again, just before reaching the 4th door, the remote is used. Now that the string is in its original state, the thief passes through the last two doors as well. The minimum number of times the remote had to be used here was 2.

You are given a file with many strings that represent the doors of the hallways. For each line, your program has to output the minimum number of times the remote needs to be used so that the thief successfully crosses the hallway.

Try to solve this in an optimal fashion paying attention to performance.

To run this program, provide the path to thief_data.csv file (thief_data_problem/thief_data.csv) in command line argument.