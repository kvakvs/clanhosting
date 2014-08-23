class ControlPanelController < ApplicationController
  before_action :pre_check_site_exists!
  before_action :pre_check_acl!

  def pre_check_site_exists!
    unless session[:clan_site_exists]
      redirect_to root_path, :alert => t('app.site_is_not_created_notice')
    end
  end

  def pre_check_acl!
    # TODO: Replace with proper ACL check
    unless session[:is_clan_leader]
      redirect_to root_path, :alert => t('acl.no_rights_to_see_this')
    end
  end

  def index

  end

  def newsfeed

  end

  def acl

  end
end
