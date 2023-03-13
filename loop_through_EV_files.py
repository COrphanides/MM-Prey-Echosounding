# -*- coding: utf-8 -*-
"""
Created on Wed Dec  2 15:01:39 2020

@author: chris.orphanides
"""
import os
import time

# Could import function this way
#from function_file import function_name
# or just put it in this script and shrink it with triangle
# I'll do teh later for now


def EVprocess_f(inputEV, inputEVdir, outCSVbaseDir, outBottomDir):
# have arguements for: inputEVdir, inputEV, outCSVbaseDir, outBottomDir
# If want to, could nest this within another loop/function that cycles through
#  the available EV files

    import win32com.client
    import os
    
    # Open EV connection
    EvApp = win32com.client.Dispatch("EchoviewCom.EvApplication")
    
        
    # If inputEV has not been assigned, give it the test datasets, paths, etc.
    try: inputEV 
    except NameError: inputEV = "HB1603_transect04_072516.EV"
    try: inputEVdir
    except NameError: inputEVdir = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1603\\"
    try: outCSVbaseDir
    except NameError: outCSVbaseDir = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\"
    try: outBottomDir
    except NameError: outBottomDir = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csv\\"
    
    # Setup filenames and directories
    cruise = inputEV[0:6]
    # Adjust transect definitions to account for transects with 3 digits
    if inputEV[17]=='_':  # i.e., if transect has 2 digits
        transect = inputEV[7:17]  
        transect_num = inputEV[15:17]
        transect_date = inputEV[18:24]
    else: # i.e, if transect has 3 digits
        transect = inputEV[7:18] 
        transect_num = inputEV[15:18]
        transect_date = inputEV[19:25]
    cruise_tran = cruise + '_' + transect + '_' 
    outCSVdir50 = outCSVbaseDir + '50x1000\\'
    outCSVdir200 = outCSVbaseDir + '200x1000\\'
    
    # Open existing EV file
    EVfilename = inputEVdir + inputEV
    
    # Set up to name output something slightly different (for now anyway)
    basename, file_ext = os.path.splitext(inputEV)
    EVfilename_out = inputEVdir + "v2\\" + basename + '_v2' + file_ext
    
        
    # Change EV filename variable called from a function so it is not hard coded
    outBottomFN = outBottomDir + 'bottom_t' + transect_num + '_' + transect_date + '.line.csv'
    # Here for testing
    try: EVfilename 
    except NameError: EVfilename = 'c:/chris/tests/testev.EV' 
    EvFile = EvApp.OpenFile(EVfilename)
    
    ##################################
    # 2) Export bottom csv
    # (when revise, name output like: "bottom_t02_080813.line.csv")
    bottomLine = EvFile.Lines.FindByName("bottom")
    varAc = EvFile.Variables.FindByName("18 Below Bottom Removed")
    # Change bottom filename variable called from a function so it is not hard coded
    # varAc.ExportLine(bottomLine,"c:/chris/tests/test_bottom.csv",-1,-1)
    varAc.ExportLine(bottomLine, outBottomFN, -1,-1)
    
    ##############################################################
    # 1) Create raw 38 w/grid at 200m to 1200 m
    # Create raw 38 kHz Echogogram w/grid at 200m to 1200m
    # identify the parent variable for the new operator
    parentVar = EvFile.Variables.FindByName("38 Below Bottom Removed")
    # enum eOperator 63 corresponds to a processed data operator 
    newVar = parentVar.AddVariable(63) 
    # set the parent variable as the operand for the new operator
    newVar.SetOperand(1,parentVar) 
    # Give it a NAME - skip, this is squirely, doesn't work consistently
    #   For now, was created as "Processed data 1"
    #   .SetName works for some reason if it is run separately, either in the console,
    #   or just hitting F9 for that line. But, if it is run together as part of a 
    #   larger section of code, then it raises and AttributeError:
    #   __getattr__  raise AttributeError("%s.%s" % (self._username_, attr))
    #  ... I am not enough of a Python expert to track that down 
    #   According to Echoview folks, SetName shouldn't work, and .Name should,
    #   but it doesn't. It provides an error: TypeError: 'str' object is not callable
    # newVar.SetName("TEST")   # This only works in certain circumstances
    # newVar.Name('TEST')   #In theory this should work, but doesn't
    newVar.Name = "38 kHz - 1200m" 
    
    
    
    #Data
    newVar.Properties.Data.ApplyMinimumThreshold = "-100"
    
    #Display
    newVar.Properties.Display.ColorMinimum= "-100"
    newVar.Properties.Display.ColorRange = "70"
    newVar.Properties.Display.AutoSetLimits = "FALSE"
    # newVar.Properties.Display.UpperLimt = "0"  # Upper limit defaults to 0
    newVar.Properties.Display.LowerLimit = "1200"
    
    
    #Grid
    # Below sets GPS distance (m) and 1000 m between grid lines
    newVar.Properties.Grid.SetTimeDistanceGrid(5, 1200)
    # Couldn't figure out COM language for setting vertical grid parameters...
    # This uses Echoview's command line options rather than the COM language
    # The below 2 lines are accomplished above using the COM language
    #EvApp.Exec("TEST | GridXAxis =| GPSDistanceM")
    #EvApp.Exec("TEST | GridXAxisSpacingInNauticalMiles  =| 0.539956800015394")
    # Set vertical grid set from watersurface every 200 m
    EvApp.Exec("38 kHz - 1200m | GridYAxisReference =| WaterSurface")
    EvApp.Exec("38 kHz - 1200m | GridYAxisSpacing =| 200.0")
    
    
    
    #Analysis
    newVar.Properties.Analysis.ExcludeAbove = "Top Bubble Layer"
    #newVar.Properties.Analysis.ExcludeBelow = "bottom" 
    #Create 1200 m line and use this as the lower analysis cutoff
    EvLine = EvFile.Lines.CreateFixedDepth(1200) # Creates "line 1" by default
    EvLine.Name = '1200 m'  # Renames Line 1
    newVar.Properties.Analysis.ExcludeBelow = "1200 m"
    
    # Next Steps
    # 1) Check if schools exist 
    # 2) if schools exist, Delete existing schools
    # 3) then delete existing regions
    # 4) re-run school detection
    # 5) Make sure bitmap region uses right schools
    
    # Delete regions, then class 
    #  If any regions are leftover would be put into unclassified region
    
    # Get school class, delete school class if it exists
    delClass = EvFile.RegionClasses.FindByName("18_schools (1)")
    if  delClass: EvFile.Regions.DeleteByClass(delClass)
    # delete class as a whole after deleting regions
    delClass = EvFile.RegionClasses.FindByName("18_schools (1)")
    EvFile.RegionClasses.Delete(delClass)
    
    delClass = EvFile.RegionClasses.FindByName("38_schools (1)")
    if  delClass: EvFile.Regions.DeleteByClass(delClass)
    delClass = EvFile.RegionClasses.FindByName("38_schools (1)")
    EvFile.RegionClasses.Delete(delClass)
    
    delClass = EvFile.RegionClasses.FindByName("70_schools (1)")
    if  delClass: EvFile.Regions.DeleteByClass(delClass)
    delClass = EvFile.RegionClasses.FindByName("70_schools (1)")
    EvFile.RegionClasses.Delete(delClass)
    
    delClass = EvFile.RegionClasses.FindByName("120_schools (1)")
    if  delClass: EvFile.Regions.DeleteByClass(delClass)
    delClass = EvFile.RegionClasses.FindByName("120_schools (1)")
    EvFile.RegionClasses.Delete(delClass)
    
    delClass = EvFile.RegionClasses.FindByName("200_schools (1)")
    if  delClass: EvFile.Regions.DeleteByClass(delClass)
    delClass = EvFile.RegionClasses.FindByName("200_schools (1)")
    EvFile.RegionClasses.Delete(delClass)
    
    
    # Run school detection
    # (because there is a variable named "18_schools", name this "18_schools (1)")
    varAc = EvFile.Variables.FindByName("18 Below Bottom Removed")
    varAc.DetectSchools("18_schools (1)",-1,-1)
    
    varAc = EvFile.Variables.FindByName("38 Below Bottom Removed")
    varAc.DetectSchools("38_schools (1)",-1,-1)
    
    varAc = EvFile.Variables.FindByName("70 Below Bottom Removed")
    varAc.DetectSchools("70_schools (1)",-1,-1)
    
    varAc = EvFile.Variables.FindByName("120 Below Bottom Removed")
    varAc.DetectSchools("120_schools (1)",-1,-1)
    
    varAc = EvFile.Variables.FindByName("200 Below Bottom Removed")
    varAc.DetectSchools("200_schools (1)",-1,-1)
     	
    # Checked school detection parameters and made sure they matched
    # previous R code
    
    # 5) Make sure bitmap region uses right schools
    # Using command line functionality because not sure how to do it with COM
    
    # Make sure that mask is assigned properly
    EvApp.Exec("18_schools | Properties RegionClass =| 18_schools (1)")
    EvApp.Exec("38_schools | Properties RegionClass =| 38_schools (1)")
    EvApp.Exec("70_schools | Properties RegionClass =| 70_schools (1)")
    EvApp.Exec("120_schools | Properties RegionClass =| 120_schools (1)")
    EvApp.Exec("200_schools | Properties RegionClass =| 200_schools (1)")
    # In the above, there is a choice between using all analysis regions/schools
    # or only using those for each school. Since I will be doing this with
    # and without schools, I chose to consider only schools associated with
    # the frequency in question
    
    
    
    # SETUP FILENAMES FOR EXPORTS -
    # Exports below, MFI 200m bins Analysis set to export to 600m, MFI 50m bins set  
    # to export to 200m, raw frequency data set to export to bottom
    
    # 3A) Export MFI with schools - 200 x 1000 m
    varAc = EvFile.Variables.FindByName("MFI_38_Phyto_Sv - 200m")
    # ADD NAMING INFO INTO THIS! outfile = ...
    # Example R code: outfile = paste(outCSVdir200, cruise_tran,  transect_date, '_', '0200x01000_PHY_038kHz_CellExport', '.csv', sep='')
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_PHY_038kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_Zoo_120 - 200m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_ZOO_120kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_38 Swim Bladd Fish Sv - 200m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_SBF_038kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_200 NSB Fish Sv - 200m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_NSB_200kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_38 Except SBF Sv - 200m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_ESB_038kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    # Alternate NSB Frequencies - worth including?
    varAc = EvFile.Variables.FindByName("MFI_38 NSB Fish Sv - 200m schools")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_NSB_038kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_120 NSB Fish Sv - 200m schools")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_NSB_120kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    
    
    # 3B) Export MFI with schools - 50 x 1000 m
    varAc = EvFile.Variables.FindByName("MFI_38_Phyto_Sv - 50m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_PHY_038kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_Zoo_120 - 50m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_ZOO_120kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_38 Swim Bladd Fish Sv - 50m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_SBF_038kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_200 NSB Fish Sv - 50m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_NSB_200kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    # Never created this one, skip it
    #varAc = EvFile.Variables.FindByName("MFI_38 Except SBF Sv - 50m")
    # ADD NAMING INFO INTO THIS! outfile = ...
    #varAc.ExportIntegrationByCellsAll(outfile)
    
    # Alternate NSB Frequencies - worth including?
    varAc = EvFile.Variables.FindByName("MFI_38 NSB Fish Sv - 50m schools")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_NSB_038kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_120 NSB Fish Sv - 50m schools")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_NSB_120kHz_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    
    
    
    # 4A) Export MFI withOUT schools - 200 x 1000 m
    varAc = EvFile.Variables.FindByName("MFI_38_Phyto_Sv - 200m (1)")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_PHY_038kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_Zoo_120 - 200m (1)")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_ZOO_120kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_38 Swim Bladd Fish Sv - 200m (1)")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_SBF_038kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_200 NSB Fish Sv - 200m (1)")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_NSB_200kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_38 Except SBF Sv - 200m (1)")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_ESB_038kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    # Alternate NSB Frequencies - worth including?
    varAc = EvFile.Variables.FindByName("MFI_38 Except SBF Sv - 200m (1)")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_NSB_038kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_120 NSB Fish Sv - 200m (1)")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_NSB_120kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    
    
    # 4B) Export MFI withOUT schools - 50 x 1000 m
    varAc = EvFile.Variables.FindByName("MFI_38_Phyto_Sv - 50m (1)")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_PHY_038kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_Zoo_120 - 50m (1)")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_ZOO_120kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_38 Swim Bladd Fish Sv - 50m (1)")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_SBF_038kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_200 NSB Fish Sv - 50m (1)")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_NSB_200kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    # didn't do this one for 50m, skip
    # varAc = EvFile.Variables.FindByName("MFI_38 Except SBF Sv - 50m (1)")
    # # ADD NAMING INFO INTO THIS! outfile = ...
    # varAc.ExportIntegrationByCellsAll(outfile)
    
    # Alternate NSB Frequencies - worth including?
    varAc = EvFile.Variables.FindByName("MFI_38 NSB Fish Sv - 50m (1)")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_NSB_038kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("MFI_120 NSB Fish Sv - 50m (1)")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_NSB_120kHz_NS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    
    
    # 5) Export raw binned to 200m & 50m
    # ... might as well export to bottom and trim out in R if necessary 
    #     rather than limiting to 200 m here (in which case creating new 
    #     variable wasn't necessary)
    
    # Export 50 m bins
    varAc = EvFile.Variables.FindByName("18 Below Bottom Removed - 50 m")
    # ADD NAMING INFO INTO THIS! outfile = ...
    # Example first thoughts on filename"c:/chris/tests/test_38raw_intg_export.csv")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_018kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("38 Below Bottom Removed - 50 m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_038kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("70 Below Bottom Removed - 50 m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_070kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("120 Below Bottom Removed - 50 m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_120kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("200 Below Bottom Removed - 50 m")
    outfile = outCSVdir50 + cruise_tran + transect_date + '_' + '0050x01000_200kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    
    # Export 200 m bins
    varAc = EvFile.Variables.FindByName("    )
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_018kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("38 Below Bottom Removed - 200 m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_038kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("70 Below Bottom Removed - 200 m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_070kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("120 Below Bottom Removed - 200 m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_120kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    varAc = EvFile.Variables.FindByName("200 Below Bottom Removed - 200 m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_200kHz_RawNS_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    
    
    # 6) Export raw 38 to 1200m (binned to 200m)
    #   ... this may not be necessary, but run it anyways
    
    varAc = EvFile.Variables.FindByName("38 kHz - 1200m")
    outfile = outCSVdir200 + cruise_tran + transect_date + '_' + '0200x01000_38kHz_RawNS1200_CellExport' + '.csv'
    varAc.ExportIntegrationByCellsAll(outfile)
    
    
    
    ## RE-CHECK MFI CALCS
    ## ONCE NAMING IS APPLIED, CHECK EACH OUTPUT & MAKE SURE ALL WORKED AS PLANNED
    # OVERWRITE original EV and give it a new name?
    
    
    EvFile.SaveAs(EVfilename_out)
    EvFile.Close()
    #EvApp.Quit() # Don't want it to quit and restart Echoview each time
   
#==========================================================================
# END OF FUNCTION
#==========================================================================




#==========================================================================
# Run 2011 data

# Same thing for 2011 files
dirname = r"C:\chris\PhD\Dissertation\prey & habitat\EV files\HB1103"

EVfileList = []
# loop through file_list
for d in os.listdir(dirname):
    if d.startswith("11.EV", 22, 27):
        EVfileList.append(d)
    else: 
        if d.startswith("11.EV", 23, 28):
            EVfileList.append(d)


# Run 2011 files
for l in EVfileList:
    print(l)
    startime = time.time()
    EVprocess_f(l, 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1103\\", 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\",
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Bottom Lines\\csv\\")
    endtime = time.time()
    time_elapsed = str(round((endtime-startime)/60,2))
    print('completed in ' + time_elapsed + ' minutes' )
    

    
#==========================================================================
# Run 2016 data

dirname = r"C:\chris\PhD\Dissertation\prey & habitat\EV files\HB1603"

EVfileList = []
# loop through file_list
for d in os.listdir(dirname):
    if d.startswith("16.EV", 22, 27):
        EVfileList.append(d)
    else: 
        if d.startswith("16.EV", 23, 28):
            EVfileList.append(d)


# Run 2016 files
for l in EVfileList:
    print(l)
    startime = time.time()
    EVprocess_f(l, 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1603\\", 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\",
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Bottom Lines\\csv\\")
    endtime = time.time()
    time_elapsed = str(round((endtime-startime)/60,2))
    print('completed in ' + time_elapsed + ' minutes' )
   
    

# EVprocess_f("HB1603_transect04_072516.EV", 
#             "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1603\\", 
#             "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\",
#             "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Bottom Lines\\csv\\")

#==========================================================================
# Run 2013 data

dirname = r"C:\chris\PhD\Dissertation\prey & habitat\EV files\HB1303\updated_template"

EVfileList = []
# loop through file_list
for d in os.listdir(dirname):
    if d.startswith("13.EV", 22, 27):
        EVfileList.append(d)
    else: 
        if d.startswith("13.EV", 23, 28):
            EVfileList.append(d)

# Run 2013 files
for l in EVfileList:
    print(l)
    startime = time.time()
    EVprocess_f(l, 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1303\\updated_template\\", 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data\\",
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csv\\")
    endtime = time.time()
    time_elapsed = str(round((endtime-startime)/60,2))
    print('completed in ' + time_elapsed + ' minutes' )
    
    

#==========================================================================
# Fix a few files data

    # If it gives an error like: bottomLine = EvFile.Lines.FindByName("bottom")
    # it likely didn't find the base input file, the bottom line part is the first 
    # part that actually does any work with the file
    
EVprocess_f("HB1603_transect116_080216_test.EV", 
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1603\\tests\\", 
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\",
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Bottom Lines\\csv\\")

EVprocess_f("HB1103_transect_21_060811_v2_fix.EV", 
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1103\\v2\\fixes\\", 
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\",
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Bottom Lines\\csv\\")

EVprocess_f("HB1103_transect_21_061611_v2_fix.EV", 
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1103\\v2\\fixes\\", 
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\",
       "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Bottom Lines\\csv\\")


#==========================================================================
# Re-Run 2013 data - nothing in v2 folder (accidentally erased?)

dirname = r"C:\chris\PhD\Dissertation\prey & habitat\EV files\HB1303\updated_template"

EVfileList = []
# loop through file_list
for d in os.listdir(dirname):
    if d.startswith("13.EV", 22, 27):
        EVfileList.append(d)
    else: 
        if d.startswith("13.EV", 23, 28):
            EVfileList.append(d)

# Run 2013 files
for l in EVfileList:
    print(l)
    startime = time.time()
    EVprocess_f(l, 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1303\\updated_template\\", 
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\DataTest\\",
            "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csvTest\\")
    endtime = time.time()
    time_elapsed = str(round((endtime-startime)/60,2))
    print('completed in ' + time_elapsed + ' minutes' )
    
    
#==========================================================================
# Fix a few 2013 files data that had "bad data (empty water)" regions

startime = time.time()
EVprocess_f("HB1303_transect09_072213.EV", 
        "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1303\\updated_template\\", 
        "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\DataTest\\",
        "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csvTest\\")
    
EVprocess_f("HB1303_transect18_081213.EV", 
    "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1303\\updated_template\\", 
    "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\DataTest\\",
    "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csvTest\\")

EVprocess_f("HB1303_transect17_081513.EV", 
    "C:\\chris\\PhD\\Dissertation\\prey & habitat\\EV files\\HB1303\\updated_template\\", 
    "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\DataTest\\",
    "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csvTest\\")
        
endtime = time.time()
time_elapsed = str(round((endtime-startime)/60,2))
print('completed in ' + time_elapsed + ' minutes' )   



#### IMPORTANT NOTE!:
####  Renmaming the above "DataTest" folder to "Data2". Will direct R proceessing program to these new files
####  Did the same with output bottom data files (changed from csvTest to csv2)
    
