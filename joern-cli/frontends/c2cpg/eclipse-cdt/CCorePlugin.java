/*******************************************************************************
 * Copyright (c) 2000, 2020 IBM Corporation and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Markus Schorn (Wind River Systems)
 *     Andrew Ferguson (Symbian)
 *     Anton Leherbauer (Wind River Systems)
 *     oyvind.harboe@zylin.com - http://bugs.eclipse.org/250638
 *     Jens Elmenthaler - http://bugs.eclipse.org/173458 (camel case completion)
 *     Sergey Prigogin (Google)
 *     Alexander Fedorov (ArSysOp) - Bug 561992
 *******************************************************************************/

package org.eclipse.cdt.core;

import org.eclipse.cdt.core.model.CModelException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Version;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This is a stripped-down version of the original org.eclipse.cdt.core.CCorePlugin shadowing it and
 * providing only the functionality to get it running without all the Eclipse OSGI context.
 * Sadly, some parser internal components (e.g., the ambiguity resolving) log via this class.
 * Without a proper OSGI instantiation (which we do not have/want in Joern) we are running into
 * all kind of exceptions due to non-initialized entities (e.g., the said logging utils).
 */
public class CCorePlugin extends Plugin {

    private static final Logger logger = LoggerFactory.getLogger(CCorePlugin.class);

    public static Version getCDTFeatureVersion() {
        return null;
    }

    public static IStatus createStatus(String msg) {
        return createStatus(msg, null);
    }

    public static IStatus createStatus(String msg, Throwable e) {
        return new Status(4, "org.eclipse.cdt.core", msg, e);
    }

    public static void log(String e) {
        log(createStatus(e));
    }

    public static void log(int severity, String msg) {
        log(new Status(severity, "org.eclipse.cdt.core", msg));
    }

    public static void logStackTrace(int severity, String msg) {
        log(new Status(severity, "org.eclipse.cdt.core", msg, new Exception()));
    }

    public static void log(String message, Throwable e) {
        Throwable nestedException;
        if (e instanceof CModelException && (nestedException = ((CModelException) e).getException()) != null) {
            e = nestedException;
        }
        log(createStatus(message, e));
    }

    public static void log(Throwable e) {
        if (e instanceof CoreException) {
            IStatus status = ((CoreException) e).getStatus();
            if (status.getException() != null) {
                log(status);
            } else {
                log(createStatus("Error", e));
            }
        } else {
            String msg = e.getMessage();
            if (msg == null) {
                log("Error", e);
            } else {
                log("Error: " + msg, e);
            }
        }
    }

    public static void log(IStatus status) {
        Throwable throwable;
        if ((throwable = status.getException()) != null) {
            String msg = throwable.getMessage();
            logger.debug(msg, throwable);
        }
    }

}
