from os import sys, mkdir, path, system, chdir, remove, rename
from subprocess import check_call
from sys import exit, stdout, stderr
import lzma
import wget
from filecmp import cmp

def test(name, ext, code_page, lenient=False):
  print('{}.{}'.format(name, ext))
  if path.exists('{}.{}.yaml'.format(name, ext)):
    remove('{}.{}.yaml'.format(name, ext))
  if path.exists('{}.test.{}'.format(name, ext)):
    remove('{}.test.{}'.format(name, ext))
  if path.exists('{}.test.{}.yaml'.format(name, ext)):
    remove('{}.test.{}.yaml'.format(name, ext))
  if lenient:
    check_call(['cargo', 'run', '--release', '--', '-p', code_page, '-kvdf',
                '{}.{}'.format(name, ext)], stdout=stdout, stderr=stderr)
    rename('{}.{}.yaml'.format(name, ext), '{}.fit.{}.yaml'.format(name, ext))
    check_call(['cargo', 'run', '--release', '--', '-p', code_page, '-vf',
                '{}.fit.{}.yaml'.format(name, ext)], stdout=stdout, stderr=stderr)
    source = '{}.fit.{}'.format(name, ext)
  else:
    source = '{}.{}'.format(name, ext)
  check_call(['cargo', 'run', '--release', '--', '-p', code_page, '-kvd',
              source], stdout=stdout, stderr=stderr)
  rename('{}.yaml'.format(source), '{}.test.{}.yaml'.format(name, ext))
  check_call(['cargo', 'run', '--release', '--', '-p', code_page, '-kv',
              '{}.test.{}.yaml'.format(name, ext)], stdout=stdout, stderr=stderr)
  if not cmp(source, '{}.test.{}'.format(name, ext)):
    exit(1)

cd = path.dirname(path.realpath(__file__))
chdir(cd)

if not path.exists('test_data'):
  mkdir('test_data')

chdir('test_data')
if not path.exists('Data Files/Aleanne Armor and Clothes 1+2.esp'):
  wget.download('http://www.fullrest.ru/files/ale-clothing-v1-1c/files?fid=2379', 'ale-clothing-v1-1c.7z')
  system('7za x ale-clothing-v1-1c.7z')

if not path.exists('Data Files/TravelingMerchants-1.2_1C.esp'):
  wget.download('http://www.fullrest.ru/uploads/files/TravelingMerchants-1.2_1C.rar', 'TravelingMerchants-1.2_1C.rar')
  system('unrar x -y TravelingMerchants-1.2_1C.rar')

if not path.exists('Data Files/Animal Behaviour.esp'):
  wget.download('https://www.fullrest.ru/uploads/files/AnimalRealism.rar', 'AnimalRealism.rar')
  system('unrar x -y AnimalRealism.rar')

if not path.exists('Data Files/Morrowind.esm'):
  print('Put Morrowind.esm into test_data/Data Files')
  exit(1)

if not path.exists('Data Files/Tribunal.esm'):
  print('Put Tribunal.esm into test_data/Data Files')
  exit(1)

if not path.exists('Data Files/Bloodmoon.esm'):
  print('Put Bloodmoon.esm into test_data/Data Files')
  exit(1)

if not path.exists('Saves/Quicksave.omwsave'):
  with lzma.open('Saves/Quicksave.omwsave.xz') as f:
    content = f.read()
  with open('Saves/Quicksave.omwsave', 'wb') as f:
    f.write(content)

test('Saves/Quicksave', 'omwsave', 'un')
test('Saves/Alchemy0000', 'ess', 'ru')
test('Saves/F0000', 'ess', 'ru')
test('Data Files/Animal Behaviour', 'esp', 'ru', lenient=True)
test('Data Files/Aleanne Armor and Clothes 1+2', 'esp', 'ru')
test('Data Files/TravelingMerchants-1.2_1C', 'esp', 'ru')
test('Data Files/Morrowind', 'esm', 'ru')
test('Data Files/Tribunal', 'esm', 'ru')
test('Data Files/Bloodmoon', 'esm', 'ru')
 
print('All tests passed.')
