class Backend < ActiveRecord::Base
  def self.save(content, filename)    
    directory = "public/data/"
    filename += ".xml"
    path = File.join(directory, filename)
    File.open(path, "wb") { |f| f.write(content.read) }
    #command = "public/erlguten/mailmerging #{path} public/data/userdata.xml" 
    #command = "public/erlguten/mailmerging public/data/ template_1.xml userdata_for_gui_test.xml" 
    #puts command
    #system command
  end

  def self.generate(filename)
    directory = "public/data/"
    filename += ".xml"
    command = "public/erlguten/mailmerging #{directory} #{filename} userdata_for_gui_test.xml" 
    #command = "public/erlguten/mailmerging public/data/ template_3.xml userdata_for_gui_test.xml" 
    puts command
    system command
  end

end
