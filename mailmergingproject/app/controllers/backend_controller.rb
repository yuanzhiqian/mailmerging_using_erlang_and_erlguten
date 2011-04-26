class BackendController < ApplicationController
  def saveXml
    post = Backend.save(request.body, params[:filename]) 
  end
  
  def generatePdf
    post = Backend.generate(params[:filename])
  end

end
