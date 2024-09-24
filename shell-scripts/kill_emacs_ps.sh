#!/bin/bash

# Define colors for pretty output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Print header
printf "${GREEN}%-10s %-8s %-8s %s${NC}\n" "PID" "%CPU" "%MEM" "COMMAND"

# List Emacs processes with specific columns
ps aux | grep [e]macs | awk '{printf "%-10s %-8s %-8s %s\n", $2, $3, $4, $11}' > temp.txt
cat temp.txt

# Number of processes
count=$(cat temp.txt | wc -l)

if [ $count -eq 0 ]; then
    echo "No Emacs processes found."
    exit 0
fi

# Ask user to select a process to kill
echo -e "\nEnter the PID of the Emacs process to kill or type 'exit' to quit:"
read pid

if [ "$pid" = "exit" ]; then
    echo "Exiting without action."
    exit 0
fi

# Validate that the user input is a number
if ! [[ "$pid" =~ ^[0-9]+$ ]]; then
    echo -e "${RED}Error:${NC} PID must be a number."
    exit 1
fi

# Check if the entered PID is in the list
if ! grep -q "^$pid" temp.txt; then
    echo -e "${RED}Error:${NC} PID $pid is not an Emacs process."
    exit 1
fi

# Kill selected process
kill $pid
if [ $? -eq 0 ]; then
    echo -e "${GREEN}Process $pid killed.${NC}"
else
    echo -e "${RED}Failed to kill process $pid.${NC}"
fi

# Cleanup temporary file
rm temp.txt
