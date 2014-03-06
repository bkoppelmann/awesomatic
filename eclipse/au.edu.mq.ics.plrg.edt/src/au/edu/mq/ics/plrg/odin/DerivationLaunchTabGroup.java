/*
 * Created on Apr 24, 2006
 *
 * Dialog tab for Eli launch configurations.
 */
package au.edu.mq.ics.plrg.odin;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.debug.ui.ILaunchConfigurationTabGroup;

/**
 * @author asloane
 * Copyright, 2006, Anthony Sloane.
 * 
 * Support for configuring Eli launches.
 */
public class DerivationLaunchTabGroup extends AbstractLaunchConfigurationTabGroup
			implements ILaunchConfigurationTabGroup {

	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		setTabs(new ILaunchConfigurationTab[] {
                     new DerivationTab(),
                     new CommonTab()
	            });
	}
	
}
