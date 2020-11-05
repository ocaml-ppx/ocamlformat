For every tokens in the test file, insert a comment at that position, format
then compute the diff at the token level.

File "test.ml" should contain most syntaxes.

  $ test_comments test.ml
  DIFF (inserted at 0-10):
    151: - <NL>
    10: + <NL>
    10: + <NL>
  DIFF (inserted at 7-17):
    152: - <NL>
  DIFF (inserted at 9-19):
    152: - <NL>
  DIFF (inserted at 11-21):
    152: - <NL>
  DIFF (inserted at 12-22):
    152: - <NL>
  DIFF (inserted at 13-23):
    151: - <NL>
    23: + (* toto *)
    21: - (* toto *)
    21: + <NL>
  DIFF (inserted at 14-24):
    151: - <NL>
    24: + <NL>
    24: + <NL>
  DIFF (inserted at 21-31):
    152: - <NL>
  DIFF (inserted at 23-33):
    151: - <NL>
    72: + (* toto *)
    31: - (* toto *)
  DIFF (inserted at 25-35):
    152: - <NL>
  DIFF (inserted at 28-38):
    151: - <NL>
    38: + (* toto *)
    36: - (* toto *)
    36: + <NL>
  DIFF (inserted at 31-41):
    151: - <NL>
    41: + <NL>
    41: + <NL>
  DIFF (inserted at 36-46):
    151: - <NL>
    46: + type
    46: + <NL>
    44: - (* toto *)
    31: - type
    31: + (* toto *)
  DIFF (inserted at 37-47):
    152: - <NL>
  DIFF (inserted at 38-48):
    151: - <NL>
    48: + (* toto *)
    46: - (* toto *)
    46: + <NL>
  DIFF (inserted at 41-51):
    151: - <NL>
    51: + <NL>
    51: + <NL>
  DIFF (inserted at 45-55):
    152: - <NL>
  DIFF (inserted at 47-57):
    152: - <NL>
  DIFF (inserted at 49-59):
    152: - <NL>
  DIFF (inserted at 51-61):
    152: - <NL>
  DIFF (inserted at 54-64):
    152: - <NL>
  DIFF (inserted at 55-65):
    152: - <NL>
  DIFF (inserted at 56-66):
    151: - <NL>
    66: + <NL>
    66: + (* toto *)
    64: - (* toto *)
    64: + <NL>
  DIFF (inserted at 60-70):
    152: - <NL>
  DIFF (inserted at 62-72):
    152: - <NL>
  DIFF (inserted at 68-78):
    151: - <NL>
    78: + (* toto *)
    76: - (* toto *)
    76: + <NL>
  DIFF (inserted at 71-81):
    151: - <NL>
    81: + <NL>
    81: + <NL>
  DIFF (inserted at 76-86):
    151: - <NL>
    86: + type
    86: + <NL>
    84: - (* toto *)
    71: - type
    71: + (* toto *)
  DIFF (inserted at 78-88):
    151: - <NL>
    132: + (* toto *)
    132: + <NL>
    132: + <NL>
    86: - (* toto *)
  DIFF (inserted at 80-90):
    152: - <NL>
  DIFF (inserted at 82-92):
    151: - <NL>
    109: + <NL>
    92: + <NL>
    92: + (* toto *)
    90: - (* toto *)
    90: + <NL>
    80: + |
    80: + <NL>
  DIFF (inserted at 84-94):
    152: - <NL>
  DIFF (inserted at 86-96):
    152: - <NL>
  DIFF (inserted at 89-99):
    152: - <NL>
  DIFF (inserted at 93-103):
    152: - <NL>
  DIFF (inserted at 95-105):
    152: - <NL>
  DIFF (inserted at 99-109):
    151: - <NL>
    109: + <NL>
    109: + (* toto *)
    107: - (* toto *)
    107: + <NL>
    82: + <NL>
    80: + |
    80: + <NL>
  DIFF (inserted at 101-111):
    152: - <NL>
  DIFF (inserted at 103-113):
    152: - <NL>
  DIFF (inserted at 106-116):
    151: - <NL>
    117: + (* toto *)
    114: - (* toto *)
  DIFF (inserted at 107-117):
    152: - <NL>
  DIFF (inserted at 108-118):
    152: - <NL>
  DIFF (inserted at 110-120):
    152: - <NL>
  DIFF (inserted at 113-123):
    151: - <NL>
    125: + (* toto *)
    121: - (* toto *)
  DIFF (inserted at 115-125):
    152: - <NL>
  DIFF (inserted at 116-126):
    152: - <NL>
  DIFF (inserted at 118-128):
    152: - <NL>
  DIFF (inserted at 121-131):
    152: - <NL>
  DIFF (inserted at 122-132):
    152: - <NL>
  DIFF (inserted at 123-133):
    151: - <NL>
    133: + (* toto *)
    131: - (* toto *)
    131: + <NL>
  DIFF (inserted at 126-136):
    151: - <NL>
    136: + <NL>
    136: + <NL>
  DIFF (inserted at 130-140):
    152: - <NL>
  DIFF (inserted at 132-142):
    152: - <NL>
  DIFF (inserted at 134-144):
    152: - <NL>
  DIFF (inserted at 136-146):
    152: - <NL>
  DIFF (inserted at 137-147):
    152: + end
    148: - end
    148: + (* toto *)
    145: - (* toto *)
    145: + <NL>
  DIFF (inserted at 138-148):
    152: + end
    148: - end
    148: + (* toto *)
    146: - (* toto *)
    146: + <NL>
  DIFF (inserted at 141-151):
    152: + (* toto *)
    149: - (* toto *)
    149: + <NL>
  DIFF (inserted at 142-152):
    150: + <NL>
  DIFF (inserted at 143-153):
    153: + <NL>
