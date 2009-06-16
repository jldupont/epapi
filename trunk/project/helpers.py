"""
    SCONS helper functions

    @version: 0.1
    @author:  Jean-Lou Dupont
"""
import os
import sys
from string import Template


def read_version():
    file = open('./VERSION')
    version = file.read()
    file.close()
    return version

def replace_params(path_src, path_dest, params):
    """
    Replace the parameters in the target path
    """
    file = open(path_src,"r")
    contents = file.read()
    file.close()
    
    tpl=Template(contents)
    updated_content = tpl.safe_substitute( **params )
    
    file = open(path_dest, "w")
    file.write(updated_content)
    file.close()
    
    
def adjust_control_files(params, c_path):
    """
    Replace the $version parameter in the control files
    """
    files = ['control', 'postinst', 'postrm', 'preinst', 'prerm']
    for file in files:
        path = "%s/%s" % (c_path, file)
        print "scons: adjusting [%s]" % path
        replace_params(path,path,params)

def get_gcpwd():
    path = os.path.expanduser("~")
    file = open("%s/.gcpwd" % path)
    pwd = file.read().strip()
    file.close()
    return pwd

def get_gcuser():
    path = os.path.expanduser("~")
    file = open("%s/.gcuser" % path)
    user = file.read().strip()
    file.close()
    return user

def get_contents(path):
    """
    Retrieves the contents of the file,
    substituting the environment variables (eg. ~)
    in the target path
    """ 
    _path = os.path.expanduser(path)
    file = open(_path)
    contents = file.read().strip()
    file.close()
    return contents

def safe_copytree(src, dst, symlinks=False, dir_mode=0777, skip_dirs=[], make_dirs=False):
    """
    Recursively copy a directory tree using copy2(). This function
    is meant to complement the less versatile ``shutil.copytree``.

    The destination directory may not already exist: missing directory
    paths are created on the fly with the ``dir_mode`` as mode.
    
    Directories can be skipped entirely using ``skip_dirs`` list ::
    
        ['.svn', '.doctree',]
    
    If exception(s) occur, an ``pyjld_os_Error`` is raised 
    with a list of reasons.

    If the optional symlinks flag is true, symbolic links in the
    source tree result in symbolic links in the destination tree; if
    it is false, the contents of the files pointed to by symbolic
    links are copied.
    """
    names = os.listdir(src)
    if make_dirs:
        os.makedirs(dst)
    errors = []
    for name in names:
        srcname = os.path.join(src, name)
        dstname = os.path.join(dst, name)
        try:
            if symlinks and os.path.islink(srcname):
                linkto = os.readlink(srcname)
                os.symlink(linkto, dstname)
            elif os.path.isdir(srcname):
                #JLD: skip dir?
                base_srcname = os.path.basename(srcname)
                if not base_srcname in skip_dirs:          
                    safe_copytree(srcname, dstname, 
                                  symlinks=symlinks, 
                                  dir_mode=dir_mode, 
                                  skip_dirs=skip_dirs,
                                  make_dirs=make_dirs)
            else:
                #JLD: make sure target directory exists
                safe_mkdir(dst, dir_mode)

                shutil.copy2(srcname, dstname)
            # XXX What about devices, sockets etc.?
        except (IOError, os.error), why:
            errors.append((srcname, dstname, str(why)))
        # catch the Error from the recursive copytree so that we can
        # continue with other files
        except pyjld_os_Error, err:
            errors.extend(err.args[0])
    try:
        shutil.copystat(src, dst)
    except WindowsError:
        # can't copy file access times on Windows
        pass     
    except OSError, why:
        errors.extend((src, dst, str(why)))
        
    if errors:
        raise pyjld_os_Error, errors


def recursive_chmod(path, 
                    mode=0775, 
                    do_files=True, 
                    do_dirs=True,
                    skip_files=[],
                    skip_dirs=[] ):
    """
    Recursive ``chmod``
    
    :param path: the top level starting path
    :param mode: the mode to apply
    :param do_files: to perform the operation on files
    :param do_dirs: to perform the operation on dirs
    :param skip_files: to skip files, list the basenames
    :param skip_dirs: to skip dirs, list the basenames
    """
    paths=[]
    for root, dirs, files in os.walk(path):
        if do_files:
            for filename in files:
                this_path = os.path.join(root, filename)
                base_name = os.path.basename( this_path )
                if base_name not in skip_files:
                    os.chmod(this_path, mode)
                    paths.append(this_path)
                
        if do_dirs:
            for _dir in dirs:
                this_path = os.path.join(root, _dir)
                base_name=os.path.basename(this_path)
                if base_name not in skip_dirs:
                    os.chmod(this_path, mode)
                    paths.append(this_path)
    return paths
