class Backend < ActiveRecord::Base
  def self.save(content, filename)    
    directory = "public/data"
    filename += ".xml"
    path = File.join(directory, filename)
    File.open(path, "wb") { |f| f.write(content.read) }
    command = "public/erlguten/mailmerging #{path} public/data/userdata.xml" 
    puts command
    system command
  end
end
