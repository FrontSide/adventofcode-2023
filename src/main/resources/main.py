import re

mapper = {
	"one": 1,
	"two": 2,
	"three": 3,
	"four": 4,
	"five": 5,
	"six": 6,
	"seven": 7,
	"eight": 8,
	"nine": 9
}

ftns = "|".join(mapper.keys())
reg = f"([1-9]|{ftns})"
reg2 = f"(?=(one|two|three|four|five|six|seven|eight|nine|[1-9]))"

print(reg)
print(reg2)


with open("input.txt", 'r+') as f:
	sum = 0
	for line in f.readlines():
		line = line.strip()
		matches = re.findall(reg, line)
		matches2 = re.findall(reg2, line)
		first, last = matches[0], matches[-1]
		first2, last2 = matches2[0], matches2[-1]
		first = mapper.get(first, first)
		first2 = mapper.get(first2, first2)
		last = mapper.get(last, last)
		last2 = mapper.get(last2, last2)
		i = int(f"{first}{last}")
		i2 = int(f"{first2}{last2}")
		if i != i2:
			print(f"{i} {i2} in {line}")
		sum += i

print(sum)

