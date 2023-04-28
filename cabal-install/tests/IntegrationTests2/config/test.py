import re

config_file = "/home/colton/Documents/prog/cabal/cabal-install/tests/IntegrationTests2/config/default-config"
with open(config_file) as f:
    lines = f.readlines()

options = []
for line in lines:
    match = re.match(r'  -- ([a-z]+(-[a-z]+)*): ', line)
    # match = re.match(r'^([a-z]+(-[a-z]+)*): .+\n', line)
    if match:
        options.append(match.group(1))

for option in options:
    # print(f"True @=? testComment \"{option}\" defaultConfigFile")
    # print(f"True @=? testAssigned \"{option}\" defaultConfigFile")
    print(f"True @=? testSubCommentAssigned \"{option}\" defaultConfigFile")