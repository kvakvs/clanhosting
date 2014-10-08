class Admin::DomainController < ApplicationController
  def index
    @vars = {}
    @vars[:custom_domain] = custom_domain
  end

  def create
  end

  def edit
  end

  def destroy
  end

  def custom_domain
    "#{request.protocol}yourclan.#{request.host}"
  end
end
