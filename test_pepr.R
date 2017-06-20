
source('pepr.R')

p = Project(file = "hello")


p = Project(file = "code/microtest/config/microtest_config.yaml")
p
p@file
p@samples
p@config
config(p)
go()

p

p@file

Project()

# equivalent:
cfg = yaml::yaml.load_file("code/microtest/config/microtest_config.yaml")
cfg