class BackendController < ApplicationController
  def saveXml
    post = Backend.save(request.body, params[:filename]) 
  end

end
