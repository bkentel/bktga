#! /bin/bash

DEPS_DIR="${TRAVIS_BUILD_DIR}/deps"
INCLUDE_DIR="${TRAVIS_BUILD_DIR}/include"

rm -rf ${INCLUDE_DIR}
mkdir -p ${INCLUDE_DIR}
mkdir -p ${DEPS_DIR}

# Update or clone a git repo $1 is the path $2 is the repo name.
Clone() {
    cd ${DEPS_DIR}

    if [[ -d "$2" ]]; then
        cd $2
        git pull origin master
        cd ..
    else
        git clone --depth=1 --branch=master "$1/$2.git"
    fi
}

# boost
BOOST_ORG="https://github.com/boostorg"

Clone ${BOOST_ORG} assert
Clone ${BOOST_ORG} config
Clone ${BOOST_ORG} core
Clone ${BOOST_ORG} detail
Clone ${BOOST_ORG} exception
Clone ${BOOST_ORG} move
Clone ${BOOST_ORG} mpl
Clone ${BOOST_ORG} optional
Clone ${BOOST_ORG} predef
Clone ${BOOST_ORG} preprocessor
Clone ${BOOST_ORG} static_assert
Clone ${BOOST_ORG} throw_exception
Clone ${BOOST_ORG} type_traits
Clone ${BOOST_ORG} utility

mkdir -p ${INCLUDE_DIR}/boost
for D in `find . -type d -name boost`; do
    cp -a ${D}/* ${INCLUDE_DIR}/boost
done

# catch
CATCH="https://github.com/philsquared"

Clone ${CATCH} Catch
mkdir -p ${INCLUDE_DIR}/Catch
cp -a ./Catch/include/* ${INCLUDE_DIR}/Catch

export INCLUDE=${INCLUDE_DIR}
cd ${TRAVIS_BUILD_DIR}
