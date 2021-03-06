from setuptools import setup

setup(name='namelistmanager',
      version='0.1',
      description='Manage I/O of Fortran namelists.',
      url='https://github.com/william-dawson/NameListManager',
      author='William Dawson',
      author_email='william.dawson@riken.jp',
      license='MIT',
      packages=['namelistmanager'],
      package_dir={'namelistmanager': 'namelistmanager'},
      package_data={'namelistmanager': ['data/*']},
      zip_safe=False,
      install_requires=[
          'lxml',
      ],
      entry_points={
          'console_scripts': [
              'namelistmanager = namelistmanager.__main__:main'
          ]
      },
      python_requires=">=3.0")
