from os import sys, mkdir, path, system, chdir, remove, rename
from sys import exit
import wget
from filecmp import cmp

def test(name, ext):
  print('{}.{}'.format(name, ext))
  if path.exists('{}.{}.yaml'.format(name, ext)):
    remove('{}.{}.yaml'.format(name, ext))
  if path.exists('{}.test.{}'.format(name, ext)):
    remove('{}.test.{}'.format(name, ext))
  if path.exists('{}.test.{}.yaml'.format(name, ext)):
    remove('{}.test.{}.yaml'.format(name, ext))
  system('cargo run -- -p ru -kvd "{}.{}"'.format(name, ext))
  rename('{}.{}.yaml'.format(name, ext), '{}.test.{}.yaml'.format(name, ext))
  system('cargo run -- -p ru -kv "{}.test.{}.yaml"'.format(name, ext))
  if not cmp('{}.{}'.format(name, ext), '{}.test.{}'.format(name, ext)):
    exit(1)

cd = path.dirname(path.realpath(__file__))
chdir(cd)

if not path.exists('test_data'):
  mkdir('test_data')

chdir("test_data")
if not path.exists('Data Files/Aleanne Armor and Clothes 1+2.esp'):
  wget.download('http://www.fullrest.ru/files/ale-clothing-v1-1c/files?fid=2379', 'ale-clothing-v1-1c.7z')
  system('7za x ale-clothing-v1-1c.7z')

if not path.exists('Data Files/TravelingMerchants-1.2_1C.esp'):
  wget.download('http://www.fullrest.ru/uploads/files/TravelingMerchants-1.2_1C.rar', 'TravelingMerchants-1.2_1C.rar')
  system('unrar x -y TravelingMerchants-1.2_1C.rar')

if not path.exists('Data Files/Morrowind.esm'):
  print("Put Morrowind.esm into test_data/Data Files")
  exit(1)

if not path.exists('Data Files/Tribunal.esm'):
  print("Put Tribunal.esm into test_data/Data Files")
  exit(1)

if not path.exists('Data Files/Bloodmoon.esm'):
  print("Put Bloodmoon.esm into test_data/Data Files")
  exit(1)

test('Saves/Alchemy0000', 'ess')
test('Data Files/Aleanne Armor and Clothes 1+2', 'esp')
test('Data Files/TravelingMerchants-1.2_1C', 'esp')
test('Data Files/Morrowind', 'esm')
test('Data Files/Tribunal', 'esm')
test('Data Files/Bloodmoon', 'esm')
 
print("All tests passed.")
