setTool("line");
waitForUser("Drawn the standard")
run("Set Scale...")
waitForUser("Drawn a line to measuare")

title = "Untitled";
  width=512; height=512;
  Dialog.create("Log_file");
  Dialog.addFile("New_file", File.getDefaultDir+"logs.csv");
  Dialog.show();
  route = Dialog.getString();

if (File.exists(route) != 1) {
	f = File.open(route);
	print(f, '# Longitude in milimeters');
	print(f, 'file' + "," + 'longitude');
	print(f, getInfo("image.filename") + "," + getValue("Length"));
}else{
 	File.append( getInfo("image.filename") + "," + getValue("Length"), route);
}
