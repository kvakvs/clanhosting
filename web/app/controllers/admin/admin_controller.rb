class Admin::AdminController < ApplicationController
  include ApplicationHelper
  before_action :pre_check_site_exists!

  def index
    return unless require_clan_admin
    true
  end
end
