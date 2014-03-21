//////////////////////////////////////////////////////////////////////////////////////
//
//   J/Link source code (c) 1999-2013, Wolfram Research, Inc. All rights reserved.
//
//   Use is governed by the terms of the J/Link license agreement, which can be found at
//   www.wolfram.com/solutions/mathlink/jlink.
//
//   Author: Todd Gayley
//
//////////////////////////////////////////////////////////////////////////////////////

// This is the SecurityManager used for WolframCloud. When a user launches a separate JVM with InstallJava[]
// in the online environment, we want a security manager in place that is restrictive. USe of this manager
// is specified in the InstallJava command line (see Install.java).

package com.wolfram.jlink;

import java.io.File;
import java.io.IOException;
import java.security.Permission;
import java.util.Vector;


/**
 */
public class JLinkCloudSecurityManager extends JLinkSecurityManager {

    private boolean allowExit = false;
    private boolean hasSetAllowedDirs = false;
    private Vector<String> allowedReadDirs = new Vector<String>();
    private Vector<String> allowedWriteDirs = new Vector<String>();
	private String mathematicaInstallationDir;
	
	
	public JLinkCloudSecurityManager() throws IOException {
	    // Needed to load the JLink native library. We allow read access to anything in the M top directory.
	    // This presumes that the Java executable is the one in the M layout, which needs to be enforced elsewhere
	    // (like in a kernel-level check for allowed arguments to LinkOpen).
	    String javaHome = System.getProperty("java.home");
	    mathematicaInstallationDir = new File(javaHome).getParentFile().getParentFile().getParentFile().getCanonicalPath();
        allowedReadDirs.add(mathematicaInstallationDir);
        // TODO: Temp for debugging:
        allowedReadDirs.add(new File("C:\\Users\\tgayley\\Documents\\MathJava\\JLink").getCanonicalPath());
	}
	
	
	// Look at MOnlineEValuationRequest.setProjectDirectories()  for some ideas about how to get user dirs. 
	// ALSO: "MathematicaOnline`$KernelRequestUserDirectory"
    //"MathematicaOnline`$KernelRequestReadDirectories", 
    //"MathematicaOnline`$KernelRequestCommonAreaDirectories
	public static void setAllowedDirectories(String[] readDirs, String[] writeDirs) {
	    
	    JLinkCloudSecurityManager sm = (JLinkCloudSecurityManager) System.getSecurityManager();
	    
	    // This method can only be called once in a session, as a security precaution.
	    if (sm.hasSetAllowedDirs)
	        return;
	    sm.hasSetAllowedDirs = true;
	    
        // Use getAbsolutePath() because getCanonicalPath() will trigger a read permission check
        // on the dir. But this will fail because we haven't set it in the allowed list yet.
        for (int i = 0; i < readDirs.length; i++) {
            try {
                String d = new java.io.File(readDirs[i]).getAbsolutePath();
                sm.allowedReadDirs.add(d);
            } catch (Exception e) {
                // Just ignore this dir. Will not be in the allowed list.
            }
        }

        for (int i = 0; i < writeDirs.length; i++) {
            try {
                String d = new java.io.File(writeDirs[i]).getAbsolutePath();
                sm.allowedWriteDirs.add(d);
            } catch (Exception e) {
                // Just ignore this dir. Will not be in the allowed list.
            }
        }
	}
	

	public void checkPermission(Permission perm) { 
        
        String name = perm.getName();
        String actions = perm.getActions();
        
        if (perm instanceof RuntimePermission) {
            if ("accessDeclaredMembers".equals(name))
                return;
            else if (name.startsWith("loadLibrary.")) {
                if (name.equals("loadLibrary.net") || name.equals("loadLibrary.nio") || name.equals("loadLibrary.JdbcOdbc"))
                    return;
                // Other than the allowed libs above (which are in the Java distribution, but arrive here with
                // only short names, not full paths), we require all native libs to be in the M directory.
                // Cannot call getCanonicalPath() here, as that leads to recursion. On Windows, it can look like
                // loadlibrary./C:/...  so drop a leading / if present. Also, convert all \ chars to / for
                // canonicalization.
                String path = name.substring(12).replace('\\', '/');
                if (path.startsWith("/"))
                    path = path.substring(1);
                if (!path.startsWith(mathematicaInstallationDir.replace('\\', '/')))
                    throw new SecurityException("Java code called from Wolfram Cloud cannot load native libraries.");
            }
            else if ("setSecurityManager".equals(name) || "createSecurityManager".equals(name))
                throw new SecurityException("Code called from Wolfram Cloud cannot set a SecurityManager.");
            else if (name.startsWith("exitVM") && !allowExit)
                throw new SecurityException("Java code called from Mathematica cannot call System.exit().");                
        } else if (perm instanceof java.io.FilePermission) {
            if (actions.contains("write") || actions.contains("delete")) {
                try {
                    String p = new java.io.File(name).getCanonicalPath();
                    for (String dir : allowedWriteDirs) {
                        if (p.startsWith(dir))
                            return;
                    }
                    throw new SecurityException("Reading is not permitted from file " + name);
                } catch (Exception e) {
                    throw new SecurityException("Reading is not permitted from file " + name);
                }
            } else if (actions.contains("read")) {
                try {
                    String p = new java.io.File(name).getCanonicalPath();
                    for (String dir : allowedReadDirs) {
                        if (p.startsWith(dir))
                            return;
                    }
                    throw new SecurityException("Reading is not permitted from file " + name);
                } catch (Exception e) {
                    throw new SecurityException("Reading is not permitted from file " + name);
                }
            } else if (actions.contains("execute")) {
                throw new SecurityException("Java code called from Wolfram Cloud cannot call exec(). File: " + name);
            }
        } else if (perm instanceof java.net.SocketPermission) {
            // Allow resolve to any host. Restrict connect. Forbid accept, listen.
            if (actions.contains("connect")) {
                String ip = name;
                if (!ip.startsWith("127.0.0.1"))
                    throw new SecurityException("Java code called from Wolfram Cloud cannot use TCP.");
            } else if (actions.contains("listen") || actions.contains("accept"))
                throw new SecurityException("Java code called from Wolfram Cloud cannot listen on network ports.");
        } else if (perm instanceof java.net.NetPermission) {
            if (name.equals("setProxySelector"))
                throw new SecurityException("Java code called from Wolfram Cloud cannot set a ProxySelector.");
        } else if (perm instanceof java.security.SecurityPermission) {
            // I think there are mostly benign.
            String foo = name;
        } else if (perm instanceof java.awt.AWTPermission) {
            throw new SecurityException("Code called from Wolfram Cloud cannot use AWT features.");
        }
        // Allow: java.lang.reflect.ReflectPermission, java.io.SerializablePermission, java.util.PropertyPermission
    }
    
    public void checkPermission(Permission perm, Object context) { 
        checkPermission(perm);
    }
    
    /*
    public void checkConnect(String host, int port) { 
        // -1 is a lookup of the IP addr from the host name. Seems safe...
        if (port != -1)
            throw new SecurityException("Java code called from Wolfram Cloud cannot use TCP.");
    }
    public void checkConnect(String host, int port, Object context) { 
        // -1 is a lookup of the IP addr from the host name. Seems safe...
        throw new SecurityException("Java code called from Wolfram Cloud cannot use TCP.");
    }
	*/
}	
