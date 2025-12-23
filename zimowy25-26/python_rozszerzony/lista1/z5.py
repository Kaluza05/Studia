def common_prefix(words : list[str])-> str:
    tried_first = set()

    longest_prefix = ''

    for word in words:
        curr_prefix = ''

        if not word:
            continue

        else:
            first_letter = word[0].lower()

            if first_letter in tried_first:
                continue

            else:
                valid_candidates = [other_word[1:] for other_word in words if other_word and other_word[0].lower() == first_letter]
                if len(valid_candidates) >= 3:
                    curr_prefix = first_letter + common_prefix(valid_candidates)

        if len(curr_prefix) > len(longest_prefix):
            longest_prefix = curr_prefix

        tried_first.add(first_letter)

    return longest_prefix
                
print(common_prefix(["Cyprian", "cyberotoman", "cynik", "ceniąc", "czule"]))