class Admin::AllianceController < ApplicationController
  include ApplicationHelper
  before_action :pre_check_site_exists!

  def index
    return unless require_acl('manage_forums') or require_clan_admin

    myclan = session[:user_clan]
    @vars = {}
    @vars[:alliances] = ClanModel.list_alliances(myclan) || []
    @vars[:alliance_requests] = ClanModel.list_alliance_requests(myclan) ||[]
  end

  # def new
  #   return unless require_acl('manage_forums') or require_clan_admin
  #   true
  # end
  #
  # def create
  #   return unless require_acl('manage_forums') or require_clan_admin
  #   return redirect_to new_admin_forum_path,
  #                      :alert => t('cp.forums.fill_at_least_title') if params[:title]==''
  #   clan_id = session[:user_clan]
  #   fields = {:title => params[:title],
  #             :clan_id => clan_id,
  #             :desc => params[:desc] || '' }
  #   ForumModel::create(clan_id, fields)
  #   redirect_to admin_forum_index_path
  # end
  #
  # def destroy
  #   return unless require_acl('manage_forums') or require_clan_admin
  #   ForumModel.delete(session[:user_clan], params[:id])
  #   redirect_to admin_forum_index_path, :notice => t('cp.forums.deleted')
  # end
  #
  # def edit
  #   return unless require_acl('manage_forums') or require_clan_admin
  #   @forum_model = ForumModel.read(session[:user_clan], params[:id])
  # end
  #
  # def update
  #   return unless require_acl('manage_forums') or require_clan_admin
  #   fields = {:title => params[:title],
  #             :desc => params[:desc] || ''}
  #   ForumModel.update(session[:user_clan], params[:id], fields)
  #   redirect_to admin_forum_index_path, :notice => t('cp.forums.updated')
  # end
end
