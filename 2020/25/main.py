#!/usr/bin/env python3

def main():
    # Part 1
    subj = 7
    pubkey_card = 5764801
    pubkey_door = 17807724

    pubkey_card = 9093927
    pubkey_door = 11001876

    r = 20201227

    # Bruteforce
    # s ** i == p (mod r)
    ls = 1
    i_card = 0
    while ls != pubkey_card:
        ls *= subj
        ls %= r
        i_card += 1
    print(i_card)

    ls = 1
    for _ in range(i_card):
        ls *= pubkey_door
        ls %= r

    print(ls)


main()
