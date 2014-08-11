class HomeController < ApplicationController
  def index

  end

  def wg_logged_in
    Rails.logger.info(request.env)
  end
end
